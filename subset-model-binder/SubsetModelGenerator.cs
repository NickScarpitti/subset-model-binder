using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace CommonNetFuncs.SubsetModelBinder;

[AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
public class SubsetOfAttribute(Type sourceType, bool isMvcApp = false) : Attribute
{
    public Type SourceType { get; } = sourceType;
    public bool IsMvcApp { get; } = isMvcApp;
}

[Generator]
public class SubsetValidatorGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        IncrementalValuesProvider<ClassDeclarationSyntax> classDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => IsSyntaxTargetForGeneration(s),
                transform: static (ctx, _) => GetSemanticTargetForGeneration(ctx))
            .Where(static m => m is not null)!;

        IncrementalValueProvider<(Compilation, ImmutableArray<ClassDeclarationSyntax>)> compilationAndClasses
            = context.CompilationProvider.Combine(classDeclarations.Collect());

        context.RegisterSourceOutput(compilationAndClasses, static (spc, source) => Execute(source.Item1, source.Item2, spc));
    }

    private static bool IsSyntaxTargetForGeneration(SyntaxNode node)
        => node is ClassDeclarationSyntax { AttributeLists.Count: > 0 };

    private static ClassDeclarationSyntax? GetSemanticTargetForGeneration(GeneratorSyntaxContext context)
    {
        ClassDeclarationSyntax classDeclarationSyntax = (ClassDeclarationSyntax)context.Node;
        foreach (AttributeListSyntax attributeListSyntax in classDeclarationSyntax.AttributeLists)
        {
            foreach (AttributeSyntax attributeSyntax in attributeListSyntax.Attributes)
            {
                if (context.SemanticModel.GetSymbolInfo(attributeSyntax).Symbol is IMethodSymbol attributeSymbol)
                {
                    INamedTypeSymbol attributeContainingTypeSymbol = attributeSymbol.ContainingType;
                    string fullName = attributeContainingTypeSymbol.ToDisplayString();

                    if (fullName == "BoundModelSourceGen.SubsetOfAttribute")
                    {
                        return classDeclarationSyntax;
                    }
                }
            }
        }
        return null;
    }

    private static void Execute(Compilation compilation, ImmutableArray<ClassDeclarationSyntax> classes, SourceProductionContext context)
    {
        if (classes.IsDefaultOrEmpty)
        {
            return;
        }

        // Use a ConcurrentDictionary to track reported diagnostics
        ConcurrentDictionary<string, bool> reportedDiagnostics = new();

        foreach (ClassDeclarationSyntax subsetClass in classes)
        {
            SemanticModel semanticModel = compilation.GetSemanticModel(subsetClass.SyntaxTree);

            if (semanticModel.GetDeclaredSymbol(subsetClass) is not INamedTypeSymbol subsetClassSymbol) continue;

            // Check if the class is marked as partial
            if (!subsetClass.Modifiers.Any(SyntaxKind.PartialKeyword))
            {
                //ReportDiagnostic(context, "SG0003", "Class must be partial", $"The class '{subsetClassSymbol.Name}' decorated with SubsetOf attribute must be marked as partial", subsetClass.Identifier.GetLocation());
                ReportDiagnosticOnce(context, "SG0003", "Class must be partial", $"The class '{subsetClassSymbol.Name}' decorated with SubsetOf attribute must be marked as partial", subsetClass.Identifier.GetLocation(), reportedDiagnostics);
            }

            AttributeData? subsetOfAttribute = subsetClassSymbol.GetAttributes().FirstOrDefault(a => a.AttributeClass?.Name == nameof(SubsetOfAttribute));

            if (subsetOfAttribute == null) continue;

            if (subsetOfAttribute.ConstructorArguments[0].Value is not INamedTypeSymbol originalTypeSymbol) continue;

            foreach (IPropertySymbol subsetProperty in subsetClassSymbol.GetMembers().OfType<IPropertySymbol>())
            {
                IPropertySymbol originalProperty = originalTypeSymbol.GetMembers(subsetProperty.Name).OfType<IPropertySymbol>().FirstOrDefault();

                if (originalProperty == null)
                {
                    //ReportDiagnostic(context, "SG0002", "Property not found", $"Property '{subsetProperty.Name}' is not present in the parent class '{originalTypeSymbol.Name}'", subsetProperty.Locations.FirstOrDefault());
                    ReportDiagnosticOnce(context, "SG0002", "Property not found", $"Property '{subsetProperty.Name}' is not present in the parent class '{originalTypeSymbol.Name}'", subsetProperty.Locations.FirstOrDefault(), reportedDiagnostics);
                }
                else if (!SymbolEqualityComparer.Default.Equals(subsetProperty.Type, originalProperty.Type))
                {
                    //ReportDiagnostic(context, "SG0001", "Property type mismatch",
                    //    $"Property '{subsetProperty.Name}' has a different type than in the original class '{originalTypeSymbol.Name}'. Expected: {originalProperty.Type.Name}, Found: {subsetProperty.Type.Name}",
                    //    subsetProperty.Locations.FirstOrDefault());

                    ReportDiagnosticOnce(context, "SG0001", "Property type mismatch",
                        $"Property '{subsetProperty.Name}' has a different type than in the original class '{originalTypeSymbol.Name}'. Expected: {originalProperty.Type.Name}, Found: {subsetProperty.Type.Name}",
                        subsetProperty.Locations.FirstOrDefault(), reportedDiagnostics);
                }
            }

            bool isMvcApp = subsetOfAttribute.ConstructorArguments.Length > 1 && (bool)subsetOfAttribute.ConstructorArguments[1].Value!;
            string attributeCode = GenerateAttributeCode(subsetClassSymbol, originalTypeSymbol, isMvcApp);
            context.AddSource($"{subsetClassSymbol.Name}_Attributes.g.cs", SourceText.From(attributeCode, Encoding.UTF8));
        }
    }

    private static string GenerateAttributeCode(INamedTypeSymbol subsetClassSymbol, INamedTypeSymbol originalTypeSymbol, bool isMvcApp)
    {
        StringBuilder sb = new();
        sb.AppendLine("#nullable enable");
        sb.AppendLine("using System;");
        sb.AppendLine(isMvcApp ? "using Microsoft.AspNetCore.Mvc;" : "using System.ComponentModel.DataAnnotations;");
        sb.AppendLine($"namespace {subsetClassSymbol.ContainingNamespace.ToDisplayString()}");
        sb.AppendLine("{");
        sb.AppendLine($"    [{(isMvcApp ? "ModelMetadataType" : "MetadataType")}(typeof({originalTypeSymbol.Name}))]");
        sb.AppendLine($"    public partial class {subsetClassSymbol.Name}");
        sb.AppendLine("    {");
        sb.AppendLine("    }");
        sb.AppendLine("}");

        return sb.ToString();
    }

    private static void ReportDiagnosticOnce(SourceProductionContext context, string id, string title, string message, Location? location, ConcurrentDictionary<string, bool> reportedDiagnostics)
    {
        string key = $"{id}:{location}:{message}";
        if (reportedDiagnostics.TryAdd(key, true))
        {
            DiagnosticDescriptor descriptor = new(id, title, message, nameof(SubsetValidatorGenerator), DiagnosticSeverity.Error, isEnabledByDefault: true);
            context.ReportDiagnostic(Diagnostic.Create(descriptor, location ?? Location.None));
        }
    }
}

//Less efficient ISourceGenerator
//[Generator]
//public class SubsetValidatorGenerator : ISourceGenerator
//{
//    public void Initialize(GeneratorInitializationContext context)
//    {
//        // No initialization required for this generator
//        context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
//    }

//    public void Execute(GeneratorExecutionContext context)
//    {
//        IEnumerable<ClassDeclarationSyntax> subsetClasses = context.Compilation.SyntaxTrees
//            .SelectMany(x => x.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>())
//            .Where(x => x.AttributeLists.SelectMany(y => y.Attributes).Any(y => y.Name.ToString() == "SubsetOf" || y.Name.ToString() == nameof(SubsetOfAttribute)));

//        foreach (ClassDeclarationSyntax subsetClass in subsetClasses)
//        {
//            SemanticModel semanticModel = context.Compilation.GetSemanticModel(subsetClass.SyntaxTree);

//            if (semanticModel.GetDeclaredSymbol(subsetClass) is not INamedTypeSymbol subsetClassSymbol) continue;

//            // Check if the class is marked as partial
//            if (!subsetClass.Modifiers.Any(SyntaxKind.PartialKeyword))
//            {
//                ReportDiagnostic(context, "SG0003", "Class must be partial", $"The class '{subsetClassSymbol.Name}' decorated with SubsetOf attribute must be marked as partial", subsetClass.Identifier.GetLocation());
//            }

//            AttributeData? subsetOfAttribute = subsetClassSymbol.GetAttributes().FirstOrDefault(a => a.AttributeClass?.Name == nameof(SubsetOfAttribute));

//            if (subsetOfAttribute == null) continue;

//            if (subsetOfAttribute.ConstructorArguments[0].Value is not INamedTypeSymbol originalTypeSymbol) continue;

//            List<Diagnostic> diagnostics = [];

//            foreach (IPropertySymbol subsetProperty in subsetClassSymbol.GetMembers().OfType<IPropertySymbol>())
//            {
//                IPropertySymbol originalProperty = originalTypeSymbol.GetMembers(subsetProperty.Name).OfType<IPropertySymbol>().FirstOrDefault();

//                if (originalProperty == null)
//                {
//                    ReportDiagnostic(context, "SG0002", "Property not found", $"Property '{subsetProperty.Name}' is not present in the parent class '{originalTypeSymbol.Name}'", subsetProperty.Locations.FirstOrDefault());
//                }
//                else if (!SymbolEqualityComparer.Default.Equals(subsetProperty.Type, originalProperty.Type))
//                {
//                    ReportDiagnostic(context, "SG0001", "Property type mismatch",
//                        $"Property '{subsetProperty.Name}' has a different type than in the original class '{originalTypeSymbol.Name}'. Expected: {originalProperty.Type.Name}, Found: {subsetProperty.Type.Name}",
//                        subsetProperty.Locations.FirstOrDefault());
//                }
//            }

//            foreach (Diagnostic diagnostic in diagnostics)
//            {
//                context.ReportDiagnostic(diagnostic);
//            }

//            bool isMvcApp = subsetOfAttribute.ConstructorArguments.Length > 1 && (bool)subsetOfAttribute.ConstructorArguments[1].Value!;
//            string attributeCode = GenerateAttributeCode(subsetClassSymbol, originalTypeSymbol, isMvcApp);
//            context.AddSource($"{subsetClassSymbol.Name}_Attributes.g.cs", SourceText.From(attributeCode, Encoding.UTF8));
//        }
//    }

//    private string GenerateAttributeCode(INamedTypeSymbol subsetClassSymbol, INamedTypeSymbol originalTypeSymbol, bool isMvcApp)
//    {
//        StringBuilder sb = new();
//        sb.AppendLine("#nullable enable");
//        sb.AppendLine("using System;");
//        sb.AppendLine(isMvcApp ? "using Microsoft.AspNetCore.Mvc;" : "using System.ComponentModel.DataAnnotations;");
//        sb.AppendLine($"namespace {subsetClassSymbol.ContainingNamespace.ToDisplayString()}");
//        sb.AppendLine("{");
//        sb.AppendLine($"    [{(isMvcApp ? "ModelMetadataType" : "MetadataType")}(typeof({originalTypeSymbol.Name}))]");
//        sb.AppendLine($"    public partial class {subsetClassSymbol.Name}");
//        sb.AppendLine("    {");

//        //This appends properties that have attributes
//        //foreach (IPropertySymbol subsetProperty in subsetClassSymbol.GetMembers().OfType<IPropertySymbol>())
//        //{
//        //    IPropertySymbol originalProperty = originalTypeSymbol.GetMembers(subsetProperty.Name).OfType<IPropertySymbol>().FirstOrDefault();
//        //    if (originalProperty != null)
//        //    {
//        //        System.Collections.Immutable.ImmutableArray<AttributeData> attributes = originalProperty.GetAttributes();
//        //        if (attributes.Any())
//        //        {
//        //            sb.AppendLine($"        // Attributes for {subsetProperty.Name}");
//        //            foreach (AttributeData attribute in attributes)
//        //            {
//        //                sb.AppendLine($"        {attribute.AttributeClass?.ToDisplayString()}({string.Join(", ", attribute.ConstructorArguments.Select(ca => ca.ToCSharpString()))})");
//        //            }
//        //            sb.AppendLine($"        public {subsetProperty.Type.ToDisplayString()} {subsetProperty.Name} {{ get; set; }}");
//        //            sb.AppendLine();
//        //        }
//        //    }
//        //}

//        sb.AppendLine("    }");
//        sb.AppendLine("}");

//        return sb.ToString();
//    }

//    private static void ReportDiagnostic(GeneratorExecutionContext context, string id, string title, string message, Location? location)
//    {
//        DiagnosticDescriptor descriptor = new(id, title, message, nameof(SubsetValidatorGenerator), DiagnosticSeverity.Error, isEnabledByDefault: true);
//        context.ReportDiagnostic(Diagnostic.Create(descriptor, location ?? Location.None));
//    }
//}

//public class SyntaxReceiver : ISyntaxReceiver
//{
//    public List<ClassDeclarationSyntax> CandidateClasses { get; } = [];

//    public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
//    {
//        // Any class with at least one attribute is a candidate for source generation
//        if (syntaxNode is ClassDeclarationSyntax classDeclarationSyntax && classDeclarationSyntax.AttributeLists.Count > 0)
//        {
//            CandidateClasses.Add(classDeclarationSyntax);
//        }
//    }
//}

////Works as basic check for properties that aren't in original class, but doesn't carry over attributes

////[Generator]
////public class SubsetGenerator : ISourceGenerator
////{
////    public void Initialize(GeneratorInitializationContext context)
////    {
////        // Register a syntax receiver that will be created for each generation pass
////        context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
////    }

////    public void Execute(GeneratorExecutionContext context)
////    {
////        List<(INamedTypeSymbol ClassSymbol, INamedTypeSymbol SourceType)> subsetClasses = [];

////        //foreach (SyntaxTree syntaxTree in context.Compilation.SyntaxTrees)
////        //{
////        //    SemanticModel semanticModel = context.Compilation.GetSemanticModel(syntaxTree);
////        //    foreach (ClassDeclarationSyntax classDeclaration in syntaxTree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>())
////        //    {
////        //        if (semanticModel.GetDeclaredSymbol(classDeclaration) is not INamedTypeSymbol classSymbol) continue;

////        //        AttributeData? subsetOfAttribute = classSymbol.GetAttributes().FirstOrDefault(attr => attr.AttributeClass?.Name == nameof(SubsetOfAttribute));

////        //        if (subsetOfAttribute?.ConstructorArguments[0].Value is INamedTypeSymbol sourceType)
////        //        {
////        //            // Check if the class is partial
////        //            if (!classDeclaration.Modifiers.Any(SyntaxKind.PartialKeyword))
////        //            {
////        //                ReportDiagnostic(context, "SG0003", "Class should be partial", $"The subset class '{classSymbol.Name}' should be declared as partial.", classDeclaration.Identifier.GetLocation());
////        //            }

////        //            subsetClasses.Add((classSymbol, sourceType));
////        //        }
////        //    }
////        //}

////        // retrieve the populated receiver
////        if (context.SyntaxReceiver is not SyntaxReceiver receiver) return;

////        // get the compilation and the semantic model
////        Compilation compilation = context.Compilation;

////        foreach (ClassDeclarationSyntax classDeclaration in receiver.CandidateClasses)
////        {
////            SemanticModel semanticModel = compilation.GetSemanticModel(classDeclaration.SyntaxTree);
////            INamedTypeSymbol? classSymbol = semanticModel.GetDeclaredSymbol(classDeclaration);

////            if (classSymbol == null) continue;

////            AttributeData? subsetOfAttribute = classSymbol.GetAttributes()
////                .FirstOrDefault(attr => attr.AttributeClass?.Name == nameof(SubsetOfAttribute));

////            if (subsetOfAttribute?.ConstructorArguments[0].Value is INamedTypeSymbol sourceType)
////            {
////                //// Check if the class is partial
////                //if (!classDeclaration.Modifiers.Any(SyntaxKind.PartialKeyword))
////                //{
////                //    ReportDiagnostic(context, "SG0003", "Class should be partial", $"The subset class '{classSymbol.Name}' should be declared as partial.", classDeclaration.Identifier.GetLocation());
////                //}
////                subsetClasses.Add((classSymbol, sourceType));
////            }
////        }

////        foreach ((INamedTypeSymbol classSymbol, INamedTypeSymbol sourceType) in subsetClasses)
////        {
////            List<IPropertySymbol> subsetProperties = classSymbol.GetMembers().OfType<IPropertySymbol>().ToList();
////            Dictionary<string, IPropertySymbol> sourceProperties = sourceType.GetMembers().OfType<IPropertySymbol>().ToDictionary(p => p.Name);

////            StringBuilder sourceBuilder = new();
////            sourceBuilder.AppendLine("#nullable enable");
////            sourceBuilder.AppendLine($"public partial class {classSymbol.Name}");
////            sourceBuilder.AppendLine("{");

////            foreach (IPropertySymbol subsetProperty in subsetProperties)
////            {
////                if (sourceProperties.TryGetValue(subsetProperty.Name, out IPropertySymbol? sourceProperty))
////                {
////                    if (SymbolEqualityComparer.Default.Equals(subsetProperty.Type, sourceProperty.Type))
////                    {
////                        sourceBuilder.AppendLine($"    public {sourceProperty.Type} {sourceProperty.Name} {{ get; set; }}");
////                    }
////                    else
////                    {
////                        ReportDiagnostic(context, "SG0001", "Property type mismatch", $"Property '{subsetProperty.Name}' in {classSymbol.Name} has a different type than in {sourceType.Name}", subsetProperty.Locations.FirstOrDefault());
////                    }
////                }
////                else
////                {
////                    ReportDiagnostic(context, "SG0002", "Property not found", $"Property '{subsetProperty.Name}' in {classSymbol.Name} not found in {sourceType.Name}", subsetProperty.Locations.FirstOrDefault());
////                }
////            }

////            sourceBuilder.AppendLine("}");

////            context.AddSource($"{classSymbol.Name}.g.cs", SourceText.From(sourceBuilder.ToString(), Encoding.UTF8));
////        }
////    }

////    private static void ReportDiagnostic(GeneratorExecutionContext context, string id, string title, string message, Location? location)
////    {
////        DiagnosticDescriptor descriptor = new(id, title, message, nameof(SubsetGenerator), DiagnosticSeverity.Error, isEnabledByDefault: true);
////        context.ReportDiagnostic(Diagnostic.Create(descriptor, location ?? Location.None));
////    }
////}

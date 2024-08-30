namespace Subset_Model_Binder;

[AttributeUsage(AttributeTargets.Class, Inherited = false, AllowMultiple = false)]
public class SubsetOfAttribute(Type sourceType, bool isMvcApp = false) : Attribute
{
    public Type SourceType { get; } = sourceType;
    public bool IsMvcApp { get; } = isMvcApp;
}

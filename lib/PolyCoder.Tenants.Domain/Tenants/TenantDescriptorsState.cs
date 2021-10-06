namespace PolyCoder.Tenants.Domain.Tenants
{
    public class TenantDescriptorsState
    {
        public static readonly TenantDescriptorsState Empty = new TenantDescriptorsState();

        private TenantDescriptorsState()
        {
        }

        public TenantDescriptorsState(string title)
        {
            Title = title;
        }

        public string Title { get; }

        public TenantDescriptorsState WithTitle(string title)
        {
            return new TenantDescriptorsState(title);
        }

        public void Deconstruct(out string title)
        {
            title = Title;
        }

        public override string ToString()
        {
            return $"Tenant(Title: {Title})";
        }
    }
}

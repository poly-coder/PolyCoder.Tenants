namespace PolyCoder.Tenants.Domain.Tenants
{
    public static class TenantDescriptorsAggregate
    {
        public static TenantDescriptorsState InitialState =>
            new TenantDescriptorsState("");
    }
}

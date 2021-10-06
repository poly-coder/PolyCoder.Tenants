namespace PolyCoder.Tenants.Domain.Tenants
{
    public abstract class TenantDescriptorsEventBase
    {
    }

    public class TenantDescriptorWasCreatedEvent :
        TenantDescriptorsEventBase
    {
        public TenantDescriptorWasCreatedEvent(string title)
        {
            Title = title;
        }

        public string Title { get; }

        public void Deconstruct(out string title)
        {
            title = Title;
        }

        public override string ToString()
        {
            return $"{nameof(TenantDescriptorWasCreatedEvent)}({nameof(Title)} = {Title})";
        }
    }

    public class TenantDescriptorWasUpdatedEvent :
        TenantDescriptorsEventBase
    {
        public TenantDescriptorWasUpdatedEvent(string title)
        {
            Title = title;
        }

        public string Title { get; }

        public void Deconstruct(out string title)
        {
            title = Title;
        }

        public override string ToString()
        {
            return $"{nameof(TenantDescriptorWasUpdatedEvent)}({nameof(Title)} = {Title})";
        }
    }

    public class TenantDescriptorWasDeletedEvent :
        TenantDescriptorsEventBase
    {
        public override string ToString()
        {
            return $"{nameof(TenantDescriptorWasDeletedEvent)}()";
        }
    }
}

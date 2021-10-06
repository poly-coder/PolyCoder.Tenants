namespace PolyCoder.Tenants.Domain.Tenants
{
    public abstract class TenantDescriptorsCommandBase
    {
    }

    public class CreateTenantDescriptorsCommand :
        TenantDescriptorsCommandBase
    {
        public CreateTenantDescriptorsCommand(string title)
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
            return $"{nameof(CreateTenantDescriptorsCommand)}({nameof(Title)} = {Title})";
        }
    }

    public class UpdateTenantDescriptorsCommand :
        TenantDescriptorsCommandBase
    {
        public UpdateTenantDescriptorsCommand(string title)
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
            return $"{nameof(UpdateTenantDescriptorsCommand)}({nameof(Title)} = {Title})";
        }
    }

    public class DeleteTenantDescriptorsCommand :
        TenantDescriptorsCommandBase
    {
        public override string ToString()
        {
            return $"{nameof(DeleteTenantDescriptorsCommand)}()";
        }
    }
}

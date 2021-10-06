using FluentValidation;

namespace PolyCoder.Tenants.Domain.Tenants
{
    public static class TenantDescriptorsValidations
    {
        public const int TenantDescriptorsTitleMinimumLength = 5;
        public const int TenantDescriptorsTitleMaximumLength = 80;

        public static void IsValidTenantDescriptorsTitle<TModel>(
            this IRuleBuilderInitial<TModel, string> ruleBuilder,
            ITenantsDomainStrings strings)
        {
            ruleBuilder
                .NotEmpty()
                .WithMessage(c => strings.IsRequiredFormat(strings.Title));

            ruleBuilder
                .MinimumLength(TenantDescriptorsTitleMinimumLength)
                .WithMessage(c => strings.MustNotBeShorterThanFormat(
                    TenantDescriptorsTitleMinimumLength,
                    strings.Title));

            ruleBuilder
                .MaximumLength(TenantDescriptorsTitleMaximumLength)
                .WithMessage(c => strings.MustNotBeLongerThanFormat(
                    TenantDescriptorsTitleMaximumLength,
                    strings.Title));
        }
    }

    public class CreateTenantDescriptorsCommandValidator :
        AbstractValidator<CreateTenantDescriptorsCommand>
    {
        public CreateTenantDescriptorsCommandValidator(
            ITenantsDomainStrings strings)
        {
            RuleFor(c => c.Title).IsValidTenantDescriptorsTitle(strings);
        }
    }

    public class UpdateTenantDescriptorsCommandValidator :
        AbstractValidator<UpdateTenantDescriptorsCommand>
    {
        public UpdateTenantDescriptorsCommandValidator(
            ITenantsDomainStrings strings)
        {
            RuleFor(c => c.Title).IsValidTenantDescriptorsTitle(strings);
        }
    }

    public class DeleteTenantDescriptorsCommandValidator :
        AbstractValidator<DeleteTenantDescriptorsCommand>
    {
        public DeleteTenantDescriptorsCommandValidator()
        {
        }
    }
}

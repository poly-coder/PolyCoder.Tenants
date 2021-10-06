using Microsoft.Extensions.Localization;
using System;

namespace PolyCoder.Tenants.Domain
{
    public interface ITenantsDomainStrings
    {
        string Title { get; }
        string IsRequiredFormat(string whatIsRequired);
        string MustNotBeShorterThanFormat(int minimumLength, string whatIsValidated);
        string MustNotBeLongerThanFormat(int maximumLength, string whatIsValidated);
    }

    public class TenantsDomainStrings :
        ITenantsDomainStrings
    {
        private readonly IStringLocalizer<TenantsDomain> strings;

        public TenantsDomainStrings(
            IStringLocalizer<TenantsDomain> strings)
        {
            this.strings = strings ?? throw new ArgumentNullException(nameof(strings));
        }

        public string Title => strings[nameof(Title)].Value;

        public string IsRequiredFormat(string whatIsRequired) =>
            strings["{0}IsRequired", whatIsRequired].Value;

        public string MustNotBeShorterThanFormat(int minimumLength, string whatIsValidated)
        {
            return strings["{0}MustNotBeShorterThan{1}", whatIsValidated, minimumLength].Value;
        }

        public string MustNotBeLongerThanFormat(int maximumLength, string whatIsValidated)
        {
            return strings["{0}MustNotBeLongerThan{1}", whatIsValidated, maximumLength].Value;
        }
    }
}

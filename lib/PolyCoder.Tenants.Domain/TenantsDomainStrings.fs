namespace PolyCoder.Tenants.Domain

type ITenantsDomainStrings =
  abstract mustNotBeNull: string
  abstract mustNotBeEmpty: string
  abstract mustNotBeWhiteSpace: string
  abstract mustNotBeShorterThan: int -> string
  abstract mustNotBeLongerThan: int -> string

# Exchange Rate Upload API – Interface Specification

**Service:** `ZF01_EXCHANGE_RATE_SRV`
**Type:** SAP Gateway **OData V2** (inbound / provider)
**Purpose:** Upload one or more foreign-exchange rates into SAP (table `TCURR`, via `BAPI_EXCHRATE_CREATEMULTIPLE`).
**Consumption:** SAP S/4HANA via SAP PO + ABAP program.
**Version:** 1.0

---

## 1. Endpoint & Connection

| Item | Value |
|------|-------|
| Protocol | HTTPS (REST / OData V2) |
| Base URL | `https://<host>:<port>/sap/opu/odata/sap/ZF01_EXCHANGE_RATE_SRV` |
| Service document | `GET  …/ZF01_EXCHANGE_RATE_SRV/` |
| **Metadata (EDMX schema)** | `GET  …/ZF01_EXCHANGE_RATE_SRV/$metadata` |
| Create operation | `POST …/ZF01_EXCHANGE_RATE_SRV/ExchangeRates` |
| Authentication | HTTP Basic (technical user) — or as agreed (OAuth/X.509) |
| Content types | `application/json` or `application/atom+xml` |

> The **`$metadata`** document (Section 6) is the machine-readable schema. Import it
> in SAP PO to generate the request/response structures automatically.

### 1.1 CSRF token (mandatory for POST)
OData V2 write calls require a CSRF token:
1. `GET …/ZF01_EXCHANGE_RATE_SRV/` with header `X-CSRF-Token: Fetch`
   → response returns `X-CSRF-Token: <token>` and session cookies.
2. Re-use that token (`X-CSRF-Token: <token>`) and the cookies on the `POST`.

---

## 2. Operation – Deep Insert (bulk create)

One `POST` uploads the **whole batch** of rates in a single call (deep insert):
`POST …/ExchangeRates` with the nested `ExchangeRate` collection.

| Property | Value |
|----------|-------|
| HTTP method | `POST` |
| Resource | `/ExchangeRates` |
| Request header | `Content-Type: application/json`, `X-CSRF-Token: <token>`, `Accept: application/json` |
| Body | Header entity `ExchangeRates` containing the `ExchangeRate` array |
| Transactionality | All-or-nothing (whole batch commits or rolls back together) |

---

## 3. Field Metadata (business fields)

Field names are identical across the XSD, the OData model, and SAP `BAPI1093_0`.

| # | Field | Type | Len | Mandatory | Format / Notes |
|---|-------|------|-----|:---------:|----------------|
| 1 | `RATE_TYPE`     | String | 4  | ✔ | Exchange rate type, e.g. `M` |
| 2 | `FROM_CURR`     | String | 5  | ✔ | Source currency, e.g. `USD` |
| 3 | `TO_CURRNCY`    | String | 5  | ✔ | Target currency, e.g. `INR` |
| 4 | `VALID_FROM`    | String | 10 | ✔ | Valid-from date, **`DD.MM.YYYY`** (e.g. `01.04.2025`) |
| 5 | `EXCH_RATE`     | String | 30 | ✔ | Exchange rate value, e.g. `95.03` |
| 6 | `FROM_FACTOR`   | String | 10 | ✔ | Ratio (from), e.g. `1` |
| 7 | `TO_FACTOR`     | String | 10 | ✔ | Ratio (to), e.g. `1` |
| 8 | `EXCH_RATE_V`   | String | 30 | – | Direct-quote rate (optional) |
| 9 | `FROM_FACTOR_V` | String | 10 | – | Direct ratio from (optional) |
| 10 | `TO_FACTOR_V`  | String | 10 | – | Direct ratio to (optional) |

Notes:
- All fields are transported as **strings** (`Edm.String` / `xs:string`).
- `VALID_FROM` must be **`DD.MM.YYYY`**; the service converts it internally to `YYYYMMDD`.
- The header entity carries a technical key `REQUEST_ID` which is **server-generated,
  not creatable** — the caller does **not** send it; it is returned in the response.

---

## 4. Request Payload

### 4.1 Request structure (XSD)
Logical request structure (`ExchangeRates` wrapping repeating `ExchangeRate`):

```xml
<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="ExchangeRates">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="ExchangeRate" maxOccurs="unbounded">
          <xs:complexType>
            <xs:sequence>
              <!-- Mandatory -->
              <xs:element name="RATE_TYPE"   type="xs:string"/>
              <xs:element name="FROM_CURR"   type="xs:string"/>
              <xs:element name="TO_CURRNCY"  type="xs:string"/>
              <xs:element name="VALID_FROM"  type="xs:string"/>
              <xs:element name="EXCH_RATE"   type="xs:string"/>
              <xs:element name="FROM_FACTOR" type="xs:string"/>
              <xs:element name="TO_FACTOR"   type="xs:string"/>
              <!-- Optional -->
              <xs:element name="EXCH_RATE_V"   type="xs:string" minOccurs="0"/>
              <xs:element name="FROM_FACTOR_V" type="xs:string" minOccurs="0"/>
              <xs:element name="TO_FACTOR_V"   type="xs:string" minOccurs="0"/>
            </xs:sequence>
          </xs:complexType>
        </xs:element>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>
```

### 4.2 Request example – JSON (recommended, deep insert)
```json
{
  "ExchangeRate": [
    {
      "RATE_TYPE": "M", "FROM_CURR": "INR", "TO_CURRNCY": "BRL",
      "VALID_FROM": "01.04.2025", "EXCH_RATE": "1.209",
      "FROM_FACTOR": "1", "TO_FACTOR": "1",
      "EXCH_RATE_V": "0", "FROM_FACTOR_V": "0", "TO_FACTOR_V": "0"
    },
    {
      "RATE_TYPE": "M", "FROM_CURR": "USD", "TO_CURRNCY": "INR",
      "VALID_FROM": "30.04.2026", "EXCH_RATE": "95.03",
      "FROM_FACTOR": "1", "TO_FACTOR": "1",
      "EXCH_RATE_V": "0", "FROM_FACTOR_V": "0", "TO_FACTOR_V": "0"
    }
  ]
}
```

### 4.3 Request example – XML (Atom deep insert)
```xml
<entry xmlns="http://www.w3.org/2005/Atom"
       xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices"
       xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">
  <content type="application/xml">
    <m:properties/>            <!-- header: REQUEST_ID server-generated -->
  </content>
  <link rel="http://schemas.microsoft.com/ado/2007/08/dataservices/related/ExchangeRate"
        type="application/atom+xml;type=feed" title="ExchangeRate">
    <m:inline>
      <feed>
        <entry><content type="application/xml"><m:properties>
          <d:RATE_TYPE>M</d:RATE_TYPE><d:FROM_CURR>INR</d:FROM_CURR>
          <d:TO_CURRNCY>BRL</d:TO_CURRNCY><d:VALID_FROM>01.04.2025</d:VALID_FROM>
          <d:EXCH_RATE>1.209</d:EXCH_RATE><d:FROM_FACTOR>1</d:FROM_FACTOR>
          <d:TO_FACTOR>1</d:TO_FACTOR><d:EXCH_RATE_V>0</d:EXCH_RATE_V>
          <d:FROM_FACTOR_V>0</d:FROM_FACTOR_V><d:TO_FACTOR_V>0</d:TO_FACTOR_V>
        </m:properties></content></entry>
        <!-- repeat <entry> per rate -->
      </feed>
    </m:inline>
  </link>
</entry>
```
> JSON is simpler and recommended. The `ExchangeRate` array/feed is the navigation
> collection under the `ExchangeRates` header.

---

## 5. Response Payload

### 5.1 Success – HTTP 201 Created
Returns the created header entity (with the generated `REQUEST_ID`). Example (JSON):
```json
{
  "d": {
    "__metadata": {
      "id": "…/ZF01_EXCHANGE_RATE_SRV/ExchangeRates('<uuid>')",
      "type": "ZF01_EXCHANGE_RATE_SRV.ExchangeRates"
    },
    "REQUEST_ID": "A1B2C3D4E5F6...32CHARS",
    "ExchangeRate": { "__deferred": { "uri": "…/ExchangeRates('<uuid>')/ExchangeRate" } }
  }
}
```

### 5.2 Response structure (XSD)
```xml
<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="ExchangeRatesResponse">
    <xs:complexType>
      <xs:sequence>
        <xs:element name="REQUEST_ID" type="xs:string"/>   <!-- generated key -->
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>
```

### 5.3 Error – HTTP 4xx / 5xx
On validation or BAPI failure, the batch is rolled back and an OData error is returned:
```xml
<error xmlns="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">
  <code>ZF01/000</code>
  <message xml:lang="en">Mandatory field missing for USD/INR 30.04.2026</message>
  <innererror>
    <transactionid>…</transactionid>
    <errordetails>
      <errordetail>
        <code>…</code>
        <message>…BAPI message…</message>
        <severity>error</severity>
      </errordetail>
    </errordetails>
  </innererror>
</error>
```
JSON equivalent:
```json
{ "error": { "code": "ZF01/000",
  "message": { "lang": "en", "value": "Mandatory field missing for USD/INR 30.04.2026" } } }
```

| HTTP status | Meaning |
|-------------|---------|
| 201 Created | All rates accepted and written to `TCURR` |
| 400 Bad Request | Malformed payload / mandatory field missing |
| 403 Forbidden | Missing/invalid CSRF token or authorization |
| 500 Internal Server Error | Backend/BAPI processing error (see message) |

---

## 6. OData Metadata (`$metadata` – EDMX schema)

Retrieve live from: `GET …/ZF01_EXCHANGE_RATE_SRV/$metadata`

```xml
<edmx:Edmx Version="1.0" xmlns:edmx="http://schemas.microsoft.com/ado/2007/06/edmx"
           xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata"
           xmlns:sap="http://www.sap.com/Protocols/SAPData">
  <edmx:DataServices m:DataServiceVersion="2.0">
    <Schema Namespace="ZF01_EXCHANGE_RATE_SRV"
            xmlns="http://schemas.microsoft.com/ado/2008/09/edm">

      <EntityType Name="ExchangeRate">
        <Key>
          <PropertyRef Name="RATE_TYPE"/><PropertyRef Name="FROM_CURR"/>
          <PropertyRef Name="TO_CURRNCY"/><PropertyRef Name="VALID_FROM"/>
        </Key>
        <Property Name="RATE_TYPE"     Type="Edm.String" Nullable="false" MaxLength="4"/>
        <Property Name="FROM_CURR"     Type="Edm.String" Nullable="false" MaxLength="5"/>
        <Property Name="TO_CURRNCY"    Type="Edm.String" Nullable="false" MaxLength="5"/>
        <Property Name="VALID_FROM"    Type="Edm.String" Nullable="false" MaxLength="10"/>
        <Property Name="EXCH_RATE"     Type="Edm.String" Nullable="false" MaxLength="30"/>
        <Property Name="FROM_FACTOR"   Type="Edm.String" Nullable="false" MaxLength="10"/>
        <Property Name="TO_FACTOR"     Type="Edm.String" Nullable="false" MaxLength="10"/>
        <Property Name="EXCH_RATE_V"   Type="Edm.String" MaxLength="30"/>
        <Property Name="FROM_FACTOR_V" Type="Edm.String" MaxLength="10"/>
        <Property Name="TO_FACTOR_V"   Type="Edm.String" MaxLength="10"/>
      </EntityType>

      <EntityType Name="ExchangeRates">
        <Key><PropertyRef Name="REQUEST_ID"/></Key>
        <Property Name="REQUEST_ID" Type="Edm.String" MaxLength="32" sap:creatable="false"/>
        <NavigationProperty Name="ExchangeRate"
            Relationship="ZF01_EXCHANGE_RATE_SRV.ExchangeRates_ExchangeRate"
            FromRole="FromRole_ExchangeRates_ExchangeRate"
            ToRole="ToRole_ExchangeRates_ExchangeRate"/>
      </EntityType>

      <Association Name="ExchangeRates_ExchangeRate">
        <End Type="ZF01_EXCHANGE_RATE_SRV.ExchangeRates" Multiplicity="1"
             Role="FromRole_ExchangeRates_ExchangeRate"/>
        <End Type="ZF01_EXCHANGE_RATE_SRV.ExchangeRate" Multiplicity="*"
             Role="ToRole_ExchangeRates_ExchangeRate"/>
      </Association>

      <EntityContainer Name="ZF01_EXCHANGE_RATE_SRV_Entities" m:IsDefaultEntityContainer="true">
        <EntitySet Name="ExchangeRate"  EntityType="ZF01_EXCHANGE_RATE_SRV.ExchangeRate"/>
        <EntitySet Name="ExchangeRates" EntityType="ZF01_EXCHANGE_RATE_SRV.ExchangeRates"/>
        <AssociationSet Name="ExchangeRates_ExchangeRateSet"
            Association="ZF01_EXCHANGE_RATE_SRV.ExchangeRates_ExchangeRate">
          <End EntitySet="ExchangeRates" Role="FromRole_ExchangeRates_ExchangeRate"/>
          <End EntitySet="ExchangeRate"  Role="ToRole_ExchangeRates_ExchangeRate"/>
        </AssociationSet>
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```

---

## 7. Testing Checklist (before SAP PO integration)

1. **Metadata reachable:** `GET …/$metadata` returns the EDMX in Section 6.
2. **CSRF fetch:** `GET …/` with `X-CSRF-Token: Fetch` returns a token.
3. **Single rate:** `POST …/ExchangeRates` with one `ExchangeRate` → HTTP 201.
4. **Multiple rates:** `POST …/ExchangeRates` with several `ExchangeRate` → HTTP 201, all in one call.
5. **Verify in SAP:** transaction `OB08` / table `TCURR` shows the uploaded rates.
6. **Negative test:** omit a mandatory field → HTTP 400 with the error body (Section 5.3).

Recommended tools: SAP Gateway Client (`/IWFND/GW_CLIENT`), Postman, or SAP PO test.

---

## 8. Notes for SAP PO / ABAP Consumption

- Import **`$metadata`** (Section 6) in SAP PO to auto-generate the request/response
  message types — no manual XSD build required.
- The request is a **deep insert**: header `ExchangeRates` + nested `ExchangeRate`
  collection in a single POST.
- `REQUEST_ID` is **not** sent by the caller (server-generated); it is returned in the
  response for logging/traceability.
- Date format for `VALID_FROM` is **`DD.MM.YYYY`**.
- The whole batch is atomic (all rates commit or none).

---

*Document owner: <your team> · Contact: <email> · Environment: <DEV/QA host:port>*

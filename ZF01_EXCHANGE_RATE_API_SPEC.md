# Exchange Rate Upload API – Interface Specification

**Service:** `ZF01_EXCHANGE_RATE_SRV`
**Type:** SAP Gateway **OData V2** (inbound / provider)
**Purpose:** Upload one or more foreign-exchange rates into SAP (table `TCURR`, via `BAPI_EXCHRATE_CREATEMULTIPLE`).
**Consumption:** SAP S/4HANA via SAP PO + ABAP program.
**Model:** Flat entity (single `ExchangeRate`; **no header, no REQUEST_ID**).
**Version:** 1.0

> Entity-set name below is shown as **`ExchangeRates`**. Confirm the exact name from
> the live `$metadata` (`<EntitySet Name="…">`) and use that in all URLs.

---

## 1. Endpoint & Connection

| Item | Value |
|------|-------|
| Protocol | HTTPS (REST / OData V2) |
| Base URL | `https://<host>:<port>/sap/opu/odata/sap/ZF01_EXCHANGE_RATE_SRV` |
| Service document | `GET  …/ZF01_EXCHANGE_RATE_SRV/` |
| **Metadata (EDMX schema)** | `GET  …/ZF01_EXCHANGE_RATE_SRV/$metadata` |
| Create (single) | `POST …/ZF01_EXCHANGE_RATE_SRV/ExchangeRates` |
| Create (bulk) | `POST …/ZF01_EXCHANGE_RATE_SRV/$batch` |
| Authentication | HTTP Basic (technical user) — or as agreed (OAuth/X.509) |
| Content types | `application/json` or `application/atom+xml` |

> Import **`$metadata`** (Section 6) in SAP PO to auto-generate the request/response
> structures.

### 1.1 CSRF token (mandatory for POST)
OData V2 write calls require a CSRF token:
1. `GET …/ZF01_EXCHANGE_RATE_SRV/` with header `X-CSRF-Token: Fetch`
   → response returns `X-CSRF-Token: <token>` + session cookies.
2. Re-use that token (`X-CSRF-Token: <token>`) and the cookies on the `POST`.

---

## 2. Operations

The service exposes **one flat entity** `ExchangeRate`. Each create writes **one** rate.

| Operation | Method | Resource | Use |
|-----------|--------|----------|-----|
| Create single rate | `POST` | `/ExchangeRates` | one `ExchangeRate` per call |
| Create multiple rates | `POST` | `/$batch` | many `ExchangeRate` in **one** HTTP call (ChangeSet) |

Request headers (both): `Content-Type` (see below), `X-CSRF-Token: <token>`, `Accept: application/json`.

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
- Key fields (composite): `RATE_TYPE`, `FROM_CURR`, `TO_CURRNCY`, `VALID_FROM`.
- `VALID_FROM` must be **`DD.MM.YYYY`**; the service converts it internally to `YYYYMMDD`.
- **No `REQUEST_ID` / no header field** — the payload contains only the 10 business fields.

---

## 4. Request Payload

### 4.1 Single-entity structure (XSD)
One `ExchangeRate` record per create:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="ExchangeRate">
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
</xs:schema>
```

### 4.2 Single create – JSON
`POST …/ExchangeRates`
```json
{
  "RATE_TYPE": "M", "FROM_CURR": "INR", "TO_CURRNCY": "BRL",
  "VALID_FROM": "01.04.2025", "EXCH_RATE": "1.209",
  "FROM_FACTOR": "1", "TO_FACTOR": "1",
  "EXCH_RATE_V": "0", "FROM_FACTOR_V": "0", "TO_FACTOR_V": "0"
}
```
Header: `Content-Type: application/json`

### 4.3 Single create – XML (Atom)
`POST …/ExchangeRates`
```xml
<entry xmlns="http://www.w3.org/2005/Atom"
       xmlns:d="http://schemas.microsoft.com/ado/2007/08/dataservices"
       xmlns:m="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">
  <content type="application/xml">
    <m:properties>
      <d:RATE_TYPE>M</d:RATE_TYPE><d:FROM_CURR>INR</d:FROM_CURR>
      <d:TO_CURRNCY>BRL</d:TO_CURRNCY><d:VALID_FROM>01.04.2025</d:VALID_FROM>
      <d:EXCH_RATE>1.209</d:EXCH_RATE><d:FROM_FACTOR>1</d:FROM_FACTOR>
      <d:TO_FACTOR>1</d:TO_FACTOR><d:EXCH_RATE_V>0</d:EXCH_RATE_V>
      <d:FROM_FACTOR_V>0</d:FROM_FACTOR_V><d:TO_FACTOR_V>0</d:TO_FACTOR_V>
    </m:properties>
  </content>
</entry>
```
Header: `Content-Type: application/atom+xml`

### 4.4 Bulk create – `$batch` (multiple rates in one call)
`POST …/$batch`
Header: `Content-Type: multipart/mixed; boundary=batch_1`, `X-CSRF-Token: <token>`

```
--batch_1
Content-Type: multipart/mixed; boundary=changeset_1

--changeset_1
Content-Type: application/http
Content-Transfer-Encoding: binary

POST ExchangeRates HTTP/1.1
Content-Type: application/json

{"RATE_TYPE":"M","FROM_CURR":"INR","TO_CURRNCY":"BRL","VALID_FROM":"01.04.2025","EXCH_RATE":"1.209","FROM_FACTOR":"1","TO_FACTOR":"1","EXCH_RATE_V":"0","FROM_FACTOR_V":"0","TO_FACTOR_V":"0"}

--changeset_1
Content-Type: application/http
Content-Transfer-Encoding: binary

POST ExchangeRates HTTP/1.1
Content-Type: application/json

{"RATE_TYPE":"M","FROM_CURR":"USD","TO_CURRNCY":"INR","VALID_FROM":"30.04.2026","EXCH_RATE":"95.03","FROM_FACTOR":"1","TO_FACTOR":"1","EXCH_RATE_V":"0","FROM_FACTOR_V":"0","TO_FACTOR_V":"0"}

--changeset_1--

--batch_1--
```
- Each `POST ExchangeRates` inside the ChangeSet = one rate.
- One ChangeSet = atomic (all succeed or all roll back).
- SAP PO / the OData V2 receiver builds this envelope automatically when **Batch Processing** is enabled.

---

## 5. Response Payload

### 5.1 Success – single create → HTTP 201 Created
Returns the created `ExchangeRate` entity (JSON):
```json
{
  "d": {
    "__metadata": {
      "id": "…/ExchangeRates(RATE_TYPE='M',FROM_CURR='INR',TO_CURRNCY='BRL',VALID_FROM='01.04.2025')",
      "type": "ZF01_EXCHANGE_RATE_SRV.ExchangeRate"
    },
    "RATE_TYPE": "M", "FROM_CURR": "INR", "TO_CURRNCY": "BRL",
    "VALID_FROM": "01.04.2025", "EXCH_RATE": "1.209",
    "FROM_FACTOR": "1", "TO_FACTOR": "1",
    "EXCH_RATE_V": "0", "FROM_FACTOR_V": "0", "TO_FACTOR_V": "0"
  }
}
```

### 5.2 Success – `$batch` → HTTP 202 Accepted
Multipart response; each ChangeSet part carries its own status:
```
--batchresponse_...
Content-Type: multipart/mixed; boundary=changesetresponse_...

--changesetresponse_...
Content-Type: application/http

HTTP/1.1 201 Created
Content-Type: application/json

{ "d": { … created ExchangeRate … } }
--changesetresponse_...--
--batchresponse_...--
```

### 5.3 Response structure (XSD)
```xml
<?xml version="1.0" encoding="UTF-8"?>
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
  <xs:element name="ExchangeRate">   <!-- echoes the created record -->
    <xs:complexType>
      <xs:sequence>
        <xs:element name="RATE_TYPE"     type="xs:string"/>
        <xs:element name="FROM_CURR"     type="xs:string"/>
        <xs:element name="TO_CURRNCY"    type="xs:string"/>
        <xs:element name="VALID_FROM"    type="xs:string"/>
        <xs:element name="EXCH_RATE"     type="xs:string"/>
        <xs:element name="FROM_FACTOR"   type="xs:string"/>
        <xs:element name="TO_FACTOR"     type="xs:string"/>
        <xs:element name="EXCH_RATE_V"   type="xs:string" minOccurs="0"/>
        <xs:element name="FROM_FACTOR_V" type="xs:string" minOccurs="0"/>
        <xs:element name="TO_FACTOR_V"   type="xs:string" minOccurs="0"/>
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema>
```

### 5.4 Error – HTTP 4xx / 5xx
On validation or BAPI failure the create is rejected:
```xml
<error xmlns="http://schemas.microsoft.com/ado/2007/08/dataservices/metadata">
  <code>ZF01/000</code>
  <message xml:lang="en">Mandatory field missing for USD/INR 30.04.2026</message>
  <innererror>
    <transactionid>…</transactionid>
    <errordetails>
      <errordetail>
        <code>…</code><message>…BAPI message…</message><severity>error</severity>
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
| 201 Created | Rate accepted and written to `TCURR` (single create) |
| 202 Accepted | `$batch` processed — check each ChangeSet part's status |
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

      <EntityContainer Name="ZF01_EXCHANGE_RATE_SRV_Entities" m:IsDefaultEntityContainer="true">
        <EntitySet Name="ExchangeRates" EntityType="ZF01_EXCHANGE_RATE_SRV.ExchangeRate"
                   sap:creatable="true" sap:updatable="false" sap:deletable="false"/>
      </EntityContainer>
    </Schema>
  </edmx:DataServices>
</edmx:Edmx>
```
> No header entity, no navigation, **no `REQUEST_ID`** — a single flat entity type/set.

---

## 7. Testing Checklist (before SAP PO integration)

1. **Metadata reachable:** `GET …/$metadata` returns the EDMX in Section 6.
2. **CSRF fetch:** `GET …/` with `X-CSRF-Token: Fetch` returns a token.
3. **Single rate:** `POST …/ExchangeRates` with one record → HTTP 201.
4. **Multiple rates:** `POST …/$batch` with a ChangeSet of several creates → HTTP 202.
5. **Verify in SAP:** transaction `OB08` / table `TCURR` shows the uploaded rates.
6. **Negative test:** omit a mandatory field → HTTP 400 with the error body (Section 5.4).

Recommended tools: SAP Gateway Client (`/IWFND/GW_CLIENT`), Postman, or SAP PO test.

---

## 8. Notes for SAP PO / ABAP Consumption

- Import **`$metadata`** (Section 6) in SAP PO to auto-generate the request/response
  message types.
- **Single record per create** (flat entity). For **bulk**, use **`$batch`** — the OData V2
  receiver in SAP PO builds the `multipart/mixed` ChangeSet automatically when
  **Batch Processing** is enabled.
- Payload contains **only the 10 business fields** — **no `REQUEST_ID`, no header**.
- Date format for `VALID_FROM` is **`DD.MM.YYYY`**.
- Keys are the 4 key fields; existing rates for the same key are updated
  (`UPD_ALLOW='X'` in the BAPI).

---

*Document owner: <your team> · Contact: <email> · Environment: <DEV/QA host:port>*

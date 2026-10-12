# ZOVL_VENDOR_SYNC_SRV: import and activation guide (OVL system)

Zip: `ZOVL_VENDOR_SYNC_abapGit.zip`. It is a standalone abapGit package and contains:

| Object | Type |
|---|---|
| `ZOVL_VEN_SYNCLOG` | Table (log) |
| `ZCL_ZOVL_VENDOR_SYNC_MPC` | Class: OData model provider |
| `ZCL_ZOVL_VENDOR_SYNC_DPC` | Class: OData data provider |
| `ZCL_OVL_VENDOR_SYNC` | Class: BP create/update logic |
| package description | `package.devc.xml` |

---

## Step 1: Create a package (SE80 / SE21)
1. Go to SE80, choose **Package**, and enter for example `ZOVL_VENDOR_SYNC`. Click **Create**.
2. Enter a short text, the application component (e.g. FI-AP), and the software component `HOME` (or your Z software component). Assign a transport request.

## Step 2: Import the zip with abapGit
1. Run report **`ZABAPGIT`** (or `ZABAPGIT_STANDALONE`) in SE38.
2. Choose **New Offline** (or **+ Offline**). Name: `ZOVL_VENDOR_SYNC`. Package: `ZOVL_VENDOR_SYNC` (from step 1). Folder logic: **Full**. Create the offline repo.
3. In the repo choose **Import zip** (or **Import <<zip**) and select `ZOVL_VENDOR_SYNC_abapGit.zip`.
4. Choose **Pull**, select the transport request, and confirm all objects.
5. abapGit activates the objects. If something stays inactive, activate it in this order:
   table `ZOVL_VEN_SYNCLOG`, then `ZCL_ZOVL_VENDOR_SYNC_MPC`, then `ZCL_OVL_VENDOR_SYNC`, then `ZCL_ZOVL_VENDOR_SYNC_DPC`.
   Use SE80 → package → right-click → **Activate**, or `SE24` / `SE11`.

> If an object does not activate because of a field-name difference in your release (for example a CVI structure component), open it in SE24, double-click the error, correct the component name, and activate again.

## Step 3: Register the model and service in the backend (`/IWBEP/REG_SERVICE`)
Code-based services are not found automatically, so you register them by hand.

**3a. Model.** In transaction `/IWBEP/REG_MODEL` (or `/IWBEP/REG_SERVICE` → **Model**):
- Technical Model Name: `ZOVL_VENDOR_SYNC_MDL`, Version: `0001`
- Model Provider Class: `ZCL_ZOVL_VENDOR_SYNC_MPC`
- Click **Create**, enter a description, save, and assign the package and transport.

**3b. Service.** In transaction `/IWBEP/REG_SERVICE`:
- Technical Service Name: `ZOVL_VENDOR_SYNC_SRV`, Version: `0001`
- Data Provider Class: `ZCL_ZOVL_VENDOR_SYNC_DPC`
- Click **Create**, enter a description, save, and assign the package and transport.
- Click **Assign Model** and select `ZOVL_VENDOR_SYNC_MDL` version `0001`. Save.

## Step 4: Activate the service on the Gateway hub (`/IWFND/MAINT_SERVICE`)
1. Run `/IWFND/MAINT_SERVICE` and click **Add Service**.
2. System Alias: `LOCAL` (embedded Gateway; on a separate hub, use the alias of the OVL backend). Technical Service Name: `ZOVL_VENDOR_SYNC_SRV`. Click **Get Services**.
3. Select the service and click **Add Selected Services**. Technical service name: `ZOVL_VENDOR_SYNC_SRV`. Enter the package and transport. Confirm with ✔.
4. Back in the service list, select `ZOVL_VENDOR_SYNC_SRV` and check the bottom panel:
   - **ICF Node** must show a green light. If it doesn't, click **ICF Node → Activate**.
   - **System Aliases** must show `LOCAL`. If not, add it with **Add System Alias**.

## Step 5: Clear the caches
- Run `/IWBEP/CACHE_CLEANUP` with **Cleanup Cache for all Models** and execute.
- Run `/IWFND/CACHE_CLEANUP` with **Cleanup Cache for all Models** and execute.

## Step 6: Check the metadata
- In `/IWFND/MAINT_SERVICE`, select the service and click **SAP Gateway Client** (or run `/IWFND/GW_CLIENT`).
- Send a **GET** to `/sap/opu/odata/sap/ZOVL_VENDOR_SYNC_SRV/$metadata`.
- Expected: HTTP 200 with entity sets `VendorSet`, `CompanyCodeSet`, `BankSet`, navigation `ToCompanyCode` and `ToBank`, and `sap:creatable="true"`.

## Step 7: Settings (STVARV), type Parameter
| Name | Value |
|---|---|
| `ZOVL_VENSYNC_ZTERM` | OVL default payment term (e.g. `0001`) |
| `ZOVL_VENSYNC_AKONT` | default reconciliation account (optional) |
| `ZOVL_VENSYNC_BUKRS` / `_GROUPING` / `_KTOKK` / `_BPCAT` | only if they differ from `OVL` / `VEMP` / `VEMP` / `1` |

Also complete the functional prerequisites in `ZOVL_VENDOR_SYNC_TECH_SPEC.md` §5. The most important is that BP grouping **VEMP** has an **external** number range that covers the ONGC vendor numbers, with "same number" assigned to vendor account group VEMP.

## Step 8: Test in `/IWFND/GW_CLIENT`
1. Send a GET to `/sap/opu/odata/sap/ZOVL_VENDOR_SYNC_SRV/` with the header `X-CSRF-Token: Fetch`. The client then reuses the token.
2. Change the method to **POST** and the URI to `/sap/opu/odata/sap/ZOVL_VENDOR_SYNC_SRV/VendorSet`. Add the headers `Content-Type: application/json` and `Accept: application/json`.
3. Paste the sample payload from `ZOVL_VENDOR_SYNC_TECH_SPEC.md` §7 and click **Execute**.
4. Expected: HTTP **201** with `"STATUS":"S"` and `"ACTION":"CREATE"`. Check the result in:
   - `BP` / `XK03`: the vendor number equals the ONGC LIFNR, company code OVL exists, and the bank details are present.
   - `SE16N` → `ZOVL_VEN_SYNCLOG`: a log row exists.
5. POST the same payload again. Expected: 201 with `"ACTION":"UPDATE"`.

## Step 9: User for CPI
- Create a system user (for example `CPI_OVL_VEN`) in SU01 with user type **System**.
- Give it a role (PFCG) containing:
  - `S_SERVICE` for `ZOVL_VENDOR_SYNC_SRV`. Add it in the role menu with **Authorization Default → TADIR Service → IWSG / IWSV**.
  - `F_LFA1_BUK` with ACTVT 01 and 02, BUKRS OVL.
  - `F_LFA1_APP`, `F_LFA1_GRP` and `F_LFA1_GEN`.
  - `B_BUPA_GRP`, `B_BUPA_RLT` (FLVN00) and `B_BUPA_ATT`.
- Give CPI this URL: `https://<ovl-host>:<port>/sap/opu/odata/sap/ZOVL_VENDOR_SYNC_SRV/`. Settings: OData V2, Create on `VendorSet`, deep insert (Sub Levels 1), **one vendor per call, no batch**.

## Transport
The objects created above (the classes, table, package, and the `/IWBEP/REG_*` and `/IWFND/MAINT_SERVICE` entries) are in the transport request. In QA and PRD, step 4 (`/IWFND/MAINT_SERVICE`, Add Service) usually has to be **repeated in each system**, followed by step 5 (clear caches).

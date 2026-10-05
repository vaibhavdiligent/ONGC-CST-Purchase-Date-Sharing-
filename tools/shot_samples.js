// Screenshot the sample dashboards (run with NODE_PATH pointing at a node_modules containing playwright)
const { chromium } = require('playwright');
const path = require('path');
(async () => {
  const b = await chromium.launch({ executablePath: '/opt/pw-browsers/chromium' });
  for (const n of ['FM', 'P2P', 'O2C']) {
    const p = await b.newPage({ viewport: { width: 1380, height: 900 }, deviceScaleFactor: 1.5 });
    const f = path.resolve(__dirname, '..', 'deploy', `Sample_Dashboard_${n}.html`);
    await p.goto('file://' + f); await p.waitForTimeout(300);
    await p.screenshot({ path: f.replace('.html', '.png'), fullPage: true }); await p.close();
  }
  await b.close();
})();

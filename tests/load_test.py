"""Load test: N browsers play Round 1 at once, then form teams of 4 and play Round 2.

    pip install playwright && playwright install chromium
    python tests/load_test.py https://app.example.org/potato/ INSTRUCTOR_PASSWORD [players] [class-code]

Creates a new class (default code LOADTEST) on the server; use a code you have not used before.
Run it from a different computer than the server, or the browsers compete with the app for CPU.
"""
import asyncio, time, random, statistics, sys
from playwright.async_api import async_playwright
URL=sys.argv[1].rstrip("/")+"/"; PW=sys.argv[2]
N=int(sys.argv[3]) if len(sys.argv)>3 else 40; N-=N%4
CODE=(sys.argv[4] if len(sys.argv)>4 else "LOADTEST").upper()
lat=[]
async def join(b, name):
    ctx = await b.new_context(viewport={"width":390,"height":700}); pg = await ctx.new_page()
    await pg.goto(URL+"?code="+CODE); await pg.fill("#join_name", name); await pg.click("#join")
    await pg.wait_for_selector("#place_order", timeout=60000); return pg
async def play_solo(pg):
    for w in range(1,21):
        await pg.wait_for_selector(f"#place_order[data-week='{w}']:not([disabled])", timeout=60000)
        await asyncio.sleep(random.uniform(2,6))          # thinking time
        await pg.fill("#order_qty", str(random.randint(2,12)))
        t=time.time(); await pg.press("#order_qty","Enter")
        if w<20: await pg.wait_for_selector(f"#place_order[data-week='{w+1}']", timeout=60000)
        else: await pg.wait_for_selector("#res_orders img", timeout=120000)
        lat.append(time.time()-t)
async def play_team(pg, last):
    for w in range(1,21):
        await pg.wait_for_selector(f"#place_order[data-week='{w}']:not([disabled])", timeout=120000)
        await asyncio.sleep(random.uniform(2,6))
        await pg.fill("#order_qty", str(random.randint(2,12))); await pg.press("#order_qty","Enter")
    await pg.wait_for_selector("#res_orders img", timeout=120000)
async def main():
    async with async_playwright() as p:
        b = await p.chromium.launch()
        ic = await b.new_context(); ip = await ic.new_page()
        await ip.goto(URL+"?instructor"); await ip.fill("#admin_pw",PW); await ip.press("#admin_pw","Enter")
        await ip.wait_for_selector("#n_code", state="attached")
        if not await ip.is_visible("#n_code"): await ip.click("text=Create a new class")
        await ip.fill("#n_code",CODE); await ip.click("#n_create"); await ip.wait_for_selector(".joincode")
        t=time.time(); pages = await asyncio.gather(*[join(b,f"Student {i:02d}") for i in range(N)]); print(f"{N} joined in {time.time()-t:.1f}s")
        t=time.time(); await asyncio.gather(*[play_solo(pg) for pg in pages])
        print(f"solo: {N} players x 20 weeks in {time.time()-t:.1f}s; order->next screen median {statistics.median(lat)*1000:.0f} ms, p95 {sorted(lat)[int(len(lat)*.95)]*1000:.0f} ms, max {max(lat)*1000:.0f} ms")
        await ip.click("#a_toggle")
        for pg in pages: await pg.click("text=Round 2")
        t=time.time()
        for k in range(N//4):
            lead=pages[4*k]; await lead.wait_for_selector("#team_create"); await lead.fill("#team_name",f"Team {k+1}"); await lead.click("#team_create")
            await lead.wait_for_selector("text=Start the game")
            for r in (2,3,4):
                pg=pages[4*k+r-1]
                await pg.click(f".teamrow:has(h3:text-is('Team {k+1}')) .rolecard:nth-child({r}) button")
            await lead.wait_for_selector("text=Leave this team"); await asyncio.sleep(1.2)
            await lead.click("text=Start the game for the whole team")
        print(f"{N//4} teams formed in {time.time()-t:.1f}s")
        t=time.time(); await asyncio.gather(*[play_team(pg,0) for pg in pages]); print(f"team: {N//4} teams x 20 weeks in {time.time()-t:.1f}s")
        await b.close()
asyncio.run(main())

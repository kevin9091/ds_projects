import asyncio
import random
import time

# ANSI colors
c = (
    "\033[0m",  # End of colour
    "\033[36m",  # Cyan
    "\033[91m",  # Red
    "\033[35m"  # Magenta
)

async def makerandom(idx, threshold, s):
    print(c[idx+1] + f"Initiated makerandom({idx}).")
    i = random.randint(0,10)
    while i<=threshold:
        elapsed = time.perf_counter() - s
        print(c[idx+1] + f"makerandom({idx}) == {i} too low; retrying. Elapsed {elapsed:0.2f} seconds")
        await asyncio.sleep(idx + 1)
        i = random.randint(0, 10)
    print(c[idx+1] + f"---> Finished: makerandom({idx}) == {i}" + c[0])
    return i

async def main():
    s = time.perf_counter()
    res = await asyncio.gather(*(makerandom(i, 10-i-1, s) for i in range(3)))
    return res

if __name__ == "__main__":
    r1, r2, r3 = asyncio.run(main())
    print()
    print(f"r1:{r1}, r2:{r2}, r3:{r3}")
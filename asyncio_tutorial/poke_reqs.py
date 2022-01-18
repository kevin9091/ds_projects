import requests
import time

start_time = time.time()

for number in range(101, 151):
    url = f'https://pokeapi.co/api/v2/pokemon/{number}'
    resp = requests.get(url)
    pokemon = resp.json()
    print(number, pokemon['name'])

print("--- %s seconds ---" % (time.time() - start_time))
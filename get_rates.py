import requests
import json

response = requests.get("https://api.exchangeratesapi.io/latest?symbols=USD")
usd_rates = json.loads(response.text)

print(json.dumps(usd_rates, sort_keys=True, indent=2))

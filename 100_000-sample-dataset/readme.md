
# Downloading source dataset of 100K

```py
import os
import numpy as np
import pandas as pd
from google.cloud import bigquery

client = bigquery.Client()
print("executing Big Query...")

query = """
SELECT address, bytecode, block_timestamp, block_number from `bigquery-public-data.crypto_ethereum.contracts`WHERE
DATE(block_timestamp) BETWEEN "2015-01-01" and "2025-01-31" and LENGTH(bytecode) > 4 ORDER BY RAND() LIMIT 100000;
"""

print("querying...")
df = client.query(query).to_dataframe()
fname = 'eth_contracts_2015_2025_random_sample_100K.csv'
print("converting to CSV")
df.to_csv(fname)
print("done")
```


awk 'FNR==1 && NR!=1{next;}{print}' *_final_benchmark_metrics.csv > longitudinal_full_benchmark.csv

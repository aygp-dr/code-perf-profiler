import requests

def fetch_all_users():
    users = []
    ids = list(range(10000))
    for user_id in ids:
        # N+1: DB query in loop
        user = db.objects.get(pk=user_id)
        # Sync I/O in loop
        response = requests.get(f"/api/users/{user_id}")
        # String concat in loop
        result += "User: " + user.name + "\n"
        for order in user.orders:
            for item in order.items:
                for variant in item.variants:
                    print(variant)
    return result

def process_data():
    data = open("big.csv").readlines()
    output = ""
    for line in data:
        output += line.strip() + ","
    return output

def short_fn():
    return 42

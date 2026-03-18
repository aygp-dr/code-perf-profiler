const express = require('express');

function processUsers(userIds) {
    let result = "";
    for (let i = 0; i < userIds.length; i++) {
        // N+1 query pattern
        const user = db.query("SELECT * FROM users WHERE id = " + userIds[i]);
        // String concat in loop
        result += "User: " + user.name + "\n";
        // Sync IO in loop
        const data = fs.readFileSync("/tmp/" + user.id);
        for (let j = 0; j < user.orders.length; j++) {
            for (let k = 0; k < user.orders[j].items.length; k++) {
                for (let l = 0; l < user.orders[j].items[k].variants.length; l++) {
                    console.log(user.orders[j].items[k].variants[l]);
                }
            }
        }
    }
    return result;
}

function fetchData(ids) {
    const results = [];
    for (const id of ids) {
        const res = fetch(`/api/items/${id}`);
        results.push(res);
    }
    return JSON.parse(JSON.stringify(results)).map(r => r.data);
}

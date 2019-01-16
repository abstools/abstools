AddDatasource() {
    curl -s 'http://admin:admin@localhost:3000/api/datasources' \
         -X POST \
         -H 'Content-Type: application/json;charset=UTF-8' \
         --data-binary @grafana-datasource.json
}

AddDashboard() {
    curl -s 'http://admin:admin@localhost:3000/api/dashboards/import' \
         -X POST \
         -H 'Content-Type: application/json;charset=UTF-8' \
         --data-binary @grafana-import-dashboard.json
}

if ! docker-compose ps | grep --quiet influxdbgrafana; then
    until AddDatasource && AddDashboard; do
        sleep 1
    done &
fi

docker-compose up

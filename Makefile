SPEC := 'https://stoplight.io/api/v1/projects/spacetraders/spacetraders/nodes/reference/SpaceTraders.json?fromExportButton=true&snapshotType=http_service&deref=optimizedBundle'

# SPEC := 'https://raw.githubusercontent.com/SpaceTradersAPI/api-docs/main/reference/SpaceTraders.json'

# fake phony
.PHONY: resources/spacetraders.v2.json
resources/spacetraders.v2.json:
	wget -O $@ $(SPEC)
	jq '.paths |= with_entries({"key": (if .key=="/my/ships/{shipSymbol}/transfer" then "/my/ships/{shipSymbol2}/transfer" else .key end), "value": .value})' $@ | sponge $@
	jq '.paths["/my/ships/{shipSymbol2}/transfer"].parameters[0].name |= "shipSymbol2"' $@ | sponge $@

BUNDLE := 'https://stoplight.io/api/v1/projects/spacetraders/spacetraders/nodes/reference/SpaceTraders.json?fromExportButton=true&snapshotType=http_service&deref=optimizedBundle'

spacetraders.v2.json:
	wget -O $@ $(BUNDLE)
	sed -i 's|"/my/ships/{shipSymbol}/transfer"|"/my/ships/{shipSymbol2}/transfer"|' $@


# TODO jq patch "/my/ships/{shipSymbol}/transfer" paramaters[0].name = "shipSymbol2"

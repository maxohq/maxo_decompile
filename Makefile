release:
	mix hex.publish

local:
	MIX_ENV=prod mix do compile, archive.build, archive.install

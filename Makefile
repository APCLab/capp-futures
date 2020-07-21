all:
	(cd futures; couchapp push)
	(cd vol; couchapp push)
	(cd tick; couchapp push)

GHVER=4.0
GHJAR=graphhopper-web-$(GHVER).jar

ghtools.jar: GHTools.java $(GHJAR)
	rm -rf nz
	javac -target 1.8 -source 1.8 -d . -cp $(GHJAR) GHTools.java
	jar fc $@ nz
	rm -rf nz

clean:
	rm -rf nz ghtools.jar *~

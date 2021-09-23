GHJAR=graphhopper-web-3.2.jar

ghtools.jar: GHTools.java $(GHJAR)
	rm -rf nz
	javac -target 1.8 -source 1.8 -d . -cp $(GHJAR) GHTools.java
	jar fc $@ nz
	rm -rf nz

clean:
	rm -rf nz ghtools.jar *~

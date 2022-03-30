package nz.urbanek.RGH;

import com.graphhopper.util.*;
import com.graphhopper.util.shapes.GHPoint;
import com.graphhopper.ResponsePath;
import com.graphhopper.GHResponse;

public class GHTools {
    /* this is intentionally not static since even though
       it should be, because there is a problem using custom
       loaders from static classes */
    public double[] pointList2ll(com.graphhopper.util.PointList pl) {
	int n = pl.size();
	double res[] = new double[n * 2];
	int i = 0;
	while (i < n) {
	    res[i] = pl.getLat(i);
	    res[i + n] = pl.getLon(i);
	    i++;
	}
	return res;
    }

    public ResponsePath[] getAllPaths(GHResponse response) {
	return response.getAll().toArray(new ResponsePath[0]);
    }
}

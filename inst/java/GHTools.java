package nz.urbanek.RGH;

import com.graphhopper.util.*;
import com.graphhopper.util.shapes.GHPoint;

public class GHTools {
    /* this is intentionally not static since even though
       it should be, because there is a problem using custom
       loaders from static classes */
    double[] pointList2ll(com.graphhopper.util.PointList pl) {
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
}

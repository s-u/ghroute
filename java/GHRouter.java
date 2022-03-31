package nz.urbanek.RGH;

import java.lang.ArrayIndexOutOfBoundsException;
import java.util.*;

import com.graphhopper.*;
import com.graphhopper.util.*;
import com.graphhopper.routing.Router;
import com.graphhopper.routing.DefaultWeightingFactory;
import com.graphhopper.storage.*;
import com.graphhopper.util.shapes.GHPoint;
import com.graphhopper.routing.ch.CHPreparationHandler;
import com.graphhopper.routing.lm.LMPreparationHandler;
import com.graphhopper.config.CHProfile;
import com.graphhopper.config.LMProfile;
import com.graphhopper.routing.lm.LandmarkStorage;
import com.graphhopper.config.Profile;

public class GHRouter {
    com.graphhopper.GraphHopper gh;
    com.graphhopper.routing.Router router;

    ResponsePath paths[] = null;
    GHResponse responses[] = null;
    int routes = 0;

    boolean firstOnly = true;

    public static final double NA_REAL = Double.longBitsToDouble(0x7ff00000000007a2L);

    GHRouter(com.graphhopper.GraphHopper gh) {
	this.gh = gh;
	
	/* analogous to GraphHopper.createRouter() */
	Map<String, RoutingCHGraph> chGraphs = new LinkedHashMap<>();
	CHPreparationHandler chPreparationHandler = gh.getCHPreparationHandler();	
        for (CHProfile chProfile : chPreparationHandler.getCHProfiles()) {
            String chGraphName = chPreparationHandler.getPreparation(chProfile.getProfile()).getCHConfig().getName();
            chGraphs.put(chProfile.getProfile(), gh.getGraphHopperStorage().getRoutingCHGraph(chGraphName));
        }
	LMPreparationHandler lmPreparationHandler = gh.getLMPreparationHandler();
        Map<String, LandmarkStorage> landmarks = new LinkedHashMap<>();
        for (LMProfile lmp : lmPreparationHandler.getLMProfiles()) {
            landmarks.put(lmp.getProfile(),
			  lmp.usesOtherPreparation()
			  ? lmPreparationHandler.getPreparation(lmp.getPreparationProfile()).getLandmarkStorage()
			  : lmPreparationHandler.getPreparation(lmp.getProfile()).getLandmarkStorage());
        }

	// this one is annoying, the GH API provides a wrapped version, unfortunately
	final Map<String, Profile> profilesByName = new LinkedHashMap<>();
	final List<Profile> profiles = gh.getProfiles();
	for (Profile profile : profiles) {
            Profile previous = profilesByName.put(profile.getName(), profile);
	}

	router = new Router(gh.getGraphHopperStorage(), gh.getLocationIndex(), profilesByName, gh.getPathDetailsBuilderFactory(),
			    gh.getTranslationMap(), gh.getRouterConfig(),
			    new DefaultWeightingFactory(gh.getGraphHopperStorage(), gh.getEncodingManager()),
			    chGraphs, landmarks);
    }

    public com.graphhopper.GraphHopper getGraphHopper() {
	return gh;
    }
    
    private void initResults(int n) {
	routes = n;
	paths = firstOnly ? new ResponsePath[n] : null;
	responses = firstOnly ? null : new GHResponse[n];
    }

    public void reset() {
	routes = 0;
	paths = null;
	responses = null;
    }
    
    public boolean[] routeMatrix(double[] m, String profile, boolean firstOnly) {
	this.firstOnly = firstOnly;
	initResults(m.length / 4);
	int i = 0;
	GHRequest request = new GHRequest(2);
	request.setProfile(profile);
	boolean res[] = new boolean[routes];
	while (i < routes) {
	    List<GHPoint> points = new ArrayList<>(2);
	    points.add(new GHPoint(m[i], m[i + routes]));
	    points.add(new GHPoint(m[i + routes * 2], m[i + routes * 3]));
	    request.setPoints(points);
	    GHResponse response = router.route(request);
	    res[i] = response.hasErrors();
	    if (firstOnly) {
		/* sadly, GHResponse has no way to ask isEmpty()
		   and yet getBest() will fail in that case so have to get all and check */
		List<ResponsePath> r_paths = response.getAll();
		paths[i] = (r_paths == null || r_paths.size() < 1) ? null : response.getBest();
	    } else responses[i] = response;
	    i++;
	}
	return res;
    }

    public ResponsePath[] getPaths() {
	return paths;
    }

    public GHResponse[] getResponses() {
	return responses;
    }

    public double[] getTimes() {
	double times[] = new double[routes];
	int i = 0;
	while (i < routes) {
	    ResponsePath path = firstOnly ? paths[i] : responses[i].getBest();
	    times[i++] = (path == null) ? NA_REAL : (((double)path.getTime()) / 1000.0);
	}
	return times;
    }

    public double[] getDistances() {
	double dist[] = new double[routes];
	int i = 0;
	while (i < routes) {
	    ResponsePath path = firstOnly ? paths[i] : responses[i].getBest();
	    dist[i++] = (path == null) ? NA_REAL : path.getDistance();
	}
	return dist;
    }

    /* may return null if there are no paths */
    public final ResponsePath getBestPath(int route) {
	if (firstOnly)
	    return paths[route];
	List<ResponsePath> paths = responses[route].getAll();
	return (paths == null || paths.size() < 1) ? null : paths.get(0);
    }

    public double[] getPoints(int route) throws ArrayIndexOutOfBoundsException {
	if (route < 0 || route >= routes) throw new ArrayIndexOutOfBoundsException(route);
	ResponsePath path = getBestPath(route);
	if (path == null)
	    return new double[0];
	PointList pl = path.getPoints();
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

    public double[] getAllPoints() {
	int N = 0;
	int i = 0;
	/* count the total number of points */
	while (i < routes) {
	    ResponsePath path = getBestPath(i);
	    if (path != null)
		N += path.getPoints().size();
	    i++;
	}
	double res[] = new double[N * 3];
	i = 0;
	int j = 0;
	while (i < routes) {
	    ResponsePath path = getBestPath(i);
	    i++;
	    if (path != null) {
		PointList pl = path.getPoints();
		int pn = pl.size();
		int k = 0;
		while(k < pn) {
		    res[j] = pl.getLat(k);
		    res[j + N] = pl.getLon(k);
		    res[j + 2 * N] = i;
		    k++;
		    j++;
		}
	    }
	}
	return res;
    }
}

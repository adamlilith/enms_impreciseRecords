areaFromPointsOrPoly <- function(x) {

	#' Area of a spatial polygon or set of points
	#'
	#' This function returns the area of a SpatialPolygon or SpatialPolygonsDataFrame object \emph{or} of the minimum convex polygon of a set of points. Input can be a set of coordinates or a polygon.
	#' @param x Any of: SpatialPoints, SpatialPointsDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame. Must be in an equal-area projection!
	#' @return Numeric (area in km2).
	#' @export

	# minimum convex polygon
	if (class(x) %in% c('SpatialPoints', 'SpatialPointsDataFrame')) {
		x <- adehabitatHR::mcp(x, percent=100, unout='km2')
	}

	# area of MCP
	area_m2 <- rgeos::gArea(x)
	area_km2 <- area_m2 / 1000^2

	area_km2
	
}

# calculate range size from coordinates using minimum convex polygon
mcpFromPolygons <- function(polys, pts=NULL) {

	#' Calculate minimum convex polygon from a set of polygons and points
	#'
	#' This function returns the range area (in km2) from a set of spatial polygons (and possibly points) representing locations of known occurrence of a species using a minimum convex polygon (MCP). A set of spatial points can also be added before calculation of the MCP. See Details for more information.
	#' @param polys SpatialPolygons or SpatialPolygonsDataFrame object, representing (for example) counties in which a species is known to reside. These must be in an equal-area projection!
	#' @param pts Either \code{NULL} or a \code{SpatialPoints} or \code{SpatialPointsDataFrame} object in an equal-area projection. These must be in an equal-area projection! See Details.
	#' @returns SpatialPolygons object representing a minimum convex polygon.
	#' @details: This function calculates range size (minimum convex polygon) from a set of spatial polygons in which a species is known to reside. The general idea is to identify a point in each polygon where a species is presumed to reside. In most cases this is unknown, so this function takes the most conservative approach by assuming the point lies on the border of the polygon that is closest to the centroid of the point pts, if they are provided, or if not the centroid of the polygons if only they are provided.
	#' @examples
	#' # red-bellied lemur in Madagascar
	#' # represented by points data and (pretend) Frarita-level occurrences
	#' mad <- raster::getData(name='GADM', country='MDG', level=2)
	#' madEaProj <- sp::CRS('+init=epsg:32738')
	#' mad <- sp::spTransform(mad, madEaProj)
	#' 
	#' data(lemurs)
	#' redBelly <- lemurs[lemurs$species == 'Eulemur rubriventer', ]
	#' ll <- c('longitude', 'latitude')
	#' wgs84 <- enmSdm::getCRS('wgs84', TRUE)
	#' redBelly <- sp::SpatialPoints(redBelly[ , ll], proj4string=wgs84)
	#' redBelly <- sp::spTransform(redBelly, madEaProj)
	#' 
	#' faritras <- c('Vakinankaratra', 'Amoron\'i mania', 'Haute matsiatra', 'Ihorombe', 'Vatovavy Fitovinany', 'Alaotra-Mangoro', 'Analanjirofo', 'Atsinanana', 'Analamanga', 'Itasy')
	#' polys <- mad[mad$NAME_2 %in% faritras, ]
	#' 
	#' mcpPolys <- mcpFromPolygons(polys)
	#' mcpPolysPoints <- mcpFromPolygons(polys, redBelly)
	#' 
	#' # range size in km2
	#' areaFromPointsOrPoly(redBelly)
	#' areaFromPointsOrPoly(mcpPolys)
	#' areaFromPointsOrPoly(mcpPolysPoints)
	#' 
	#' plot(mad)
	#' plot(polys, col='gray80', add=TRUE)
	#' plot(mcpPolysPoints, add=TRUE, col=scales::alpha('green', 0.4))
	#' plot(mcpPolys, add=TRUE, col=scales::alpha('purple', 0.4))
	#' points(redBelly, pch=16)
	#' legend('bottomright', legend=c('Presences', '"Occupied" Faritras', 'MCP w/ polygons', 'MCP w/ polygons & points'), fill=c(NA, 'gray', scales::alpha('purple', 0.4), scales::alpha('green', 0.4)), pch=c(16, NA, NA, NA), border=c(NA, 'black', 'black', 'black'))
	#' @export

	### useful info
		
		# type of polygons
		polyIsDf <- ('SpatialPolygonsDataFrame' %in% class(polys))
		
		# number of polygons
		numPolys <- if (polyIsDf) {
			nrow(polys)
		} else {
			length(polys)
		}
		
	### focal centroid
		
		# polygon centroids
		polyCents <- rgeos::gCentroid(polys, byid=TRUE)

		# focal centroid
		center <- if (!is.null(pts)) {
			rgeos::gCentroid(pts)
		} else {
			rgeos::gCentroid(polyCents)
		}

	### find closest points to center
		
		# stores coordinates of intersections (and later maybe also reference coordinates)
		coords <- matrix(nrow=0, ncol=2)
		colnames(coords) <- c('longitude', 'latitude')
		
		# by polygon
		for (countPoly in 1:numPolys) {
			
			# get this polygon
			thisPoly <- if (polyIsDf) {
				polys[countPoly, ]
			} else {
				polys[countPoly]
			}
			
			# find closest point on this polygon to the focal centroid
		
			closestPoints <- rgeos::gNearestPoints(center, thisPoly)
			polyIntersectPoint <- closestPoints[2]
			polyIntersectPointCoords <- sp::coordinates(polyIntersectPoint)
			coords <- rbind(coords, polyIntersectPointCoords)
		
		} # next polygon

		# add record coordinates
		if (!is.null(pts)) {
			recordCoords <- sp::coordinates(pts)
			coords <- rbind(coords, recordCoords)
		}

		rownames(coords) <- 1:nrow(coords)
		
		# spatialize
		proj4 <- raster::projection(polys)
		proj4 <- sp::CRS(proj4)
		coords <- sp::SpatialPoints(coords, proj4string=proj4)

	### MCP
		
		minConvexPoly <- adehabitatHR::mcp(coords, 100, unin='m', unout='km2')
		minConvexPoly
		
}


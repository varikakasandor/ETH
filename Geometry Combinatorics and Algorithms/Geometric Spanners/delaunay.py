from main_towns import *
from helper import *
import networkx as nx
import matplotlib.pyplot as plt
from libpysal import weights, examples
from libpysal.cg import voronoi_frames
import numpy as np

coordinates = np.array(list(main_towns.items()))
cells, generators = voronoi_frames(coordinates, clip="convex hull")
delaunay = weights.Rook.from_dataframe(cells)
delaunay_graph = delaunay.to_networkx()
positions = dict(zip(delaunay_graph.nodes, coordinates))
nx.draw(delaunay_graph, pos=positions)
nx.draw_networkx_labels(delaunay_graph, pos=positions, labels={i: list(main_towns.keys())[i] for i in range(len(main_towns))})
plt.savefig(f"delaunay_spanner.pdf", format="pdf")
plt.show()

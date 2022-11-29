from scipy.spatial import Delaunay

from helper import *
from swiss_towns import *


def delaunay_spanner():
    points = list(main_towns.values())
    delaunay = Delaunay(points)
    edges = set()
    for l in delaunay.simplices:
        for i in range(3):
            edges.add((l[i], l[(i + 1) % 3]))
    G = nx.Graph()
    for town_1, coordinates_1 in main_towns.items():
        G.add_node(town_1)
    for i, j in edges:
        G.add_edge(list(main_towns.keys())[i], list(main_towns.keys())[j],
                   distance=dist(list(main_towns.values())[i], list(main_towns.values())[j]))
    create_figure(G, main_towns, "delaunay_spanner")
    return G


if __name__ == "__main__":
    delaunay_spanner()

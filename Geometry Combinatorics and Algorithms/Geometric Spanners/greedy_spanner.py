from main_towns import *
from helper import *
import networkx as nx
import matplotlib.pyplot as plt

def greedy_spanner(t):
    G = nx.Graph()
    edges_sorted = []
    for town_1, coordinates_1 in main_towns.items():
        G.add_node(town_1)
        for town_2, coordinates_2 in main_towns.items():
            if town_1 < town_2:
                edges_sorted.append((dist(coordinates_1, coordinates_2), town_1, town_2))
    edges_sorted = sorted(edges_sorted)
    for (d, town_1, town_2) in edges_sorted:
        current_distance = None
        try:
            current_distance = nx.shortest_path_length(G, source=town_1, target=town_2, weight="distance")
        except Exception as e:
            pass
        if current_distance is None or current_distance > t * d:
            G.add_edge(town_1, town_2, distance=d)
    nx.draw(G, pos=main_towns)
    nx.draw_networkx_labels(G, pos=main_towns, labels={k:k for k in main_towns.keys()})
    plt.savefig(f"greedy_spanner_t={t}.pdf", format="pdf")
    plt.show()
    return G


if __name__ == "__main__":
    greedy_spanner(2.0)

from main_towns import *
from helper import *


def create_dilated_network():
    G = nx.Graph()
    for town in main_towns.keys():
        G.add_node(town)
    create_figure(G, main_towns, "empty_network")
    return G


def greedy_example_network():
    G = nx.Graph()
    for town_1, coordinates_1 in main_towns.items():
        G.add_node(town_1)
    G.add_edge("Bern", "Luzern")
    G.add_edge("Luzern", "Zurich")
    G.add_edge("Zurich", "St Gallen")
    G.add_edge("Lugano", "Geneva")
    G.add_edge("Geneva", "Lausanne")
    G.add_edge("Bern", "Lausanne")
    G.add_edge("Bern", "Basel")
    G.add_edge("Lugano", "St Gallen")
    nx.draw_networkx_edges(G, pos=main_towns, edgelist=[("Bern", "St Gallen")], style="dashed", edge_color="green")
    nx.draw_networkx_edges(G, pos=main_towns, edgelist=[("Bern", "Lugano")], style="dashed", edge_color="red")
    create_figure(G, main_towns, "example_network")
    return G


if __name__ == "__main__":
    # create_empty_network()
    greedy_example_network()

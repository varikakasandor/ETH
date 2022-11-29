from swiss_towns import *
from helper import *


def create_dilated_network():
    G_main, G_steiner = nx.Graph(), nx.Graph()
    for town in main_towns.keys():
        G_main.add_node(town)
    for town in steiner_towns.keys():
        G_steiner.add_node(town)
    create_figure(G_main, main_towns, "tmp", nodelist=list(main_towns.keys()), font_size=12, save=False, show=False)
    create_figure(G_steiner, steiner_towns, "dilation_network", nodelist=list(steiner_towns.keys()), font_size=6, save=True, show=True)


def create_square():
    G = nx.Graph()
    pos_dict = {
        0: (1, 1),
        1: (1, -1),
        2: (-1, -1),
        3: (-1, 1)
    }
    G.add_nodes_from(list(range(4)))
    for i in range(4):
        for j in range(i + 1, 4):
            if not (i == 0 and j == 2):
                G.add_edge(i, j)
    create_figure(G, pos_dict, "square", with_labels=False, node_size=300, node_color="black")
    return G


def create_pentagon():
    G = nx.Graph()
    pos_dict = calculate_coordinates_of_pentagon(dilate=False)
    G.add_nodes_from(list(range(5)))
    create_figure(G, pos_dict, "pentagon", with_labels=False, node_size=300, node_color="black")
    return G


def create_dilated_square():
    G = nx.Graph()
    pos_dict = {
        0: (1, 1),
        1: (1, -1),
        2: (-1, -1),
        3: (-1, 1),
        4: (0, 0)
    }
    G.add_nodes_from(list(range(5)))
    for i in range(4):
        G.add_edge(i, (i + 1) % 4)
        G.add_edge(i, 4)
    create_figure(G, pos_dict, "dilated_square", with_labels=False, node_size=300, node_color="black")
    return G



def create_dilated_pentagon():
    G = nx.Graph()
    pos_dict = calculate_coordinates_of_pentagon(dilate=True)
    G.add_nodes_from(list(range(6)))
    for i in range(5):
        G.add_edge(i, (i + 1) % 5)
        G.add_edge(i, 5)
    create_figure(G, pos_dict, "dilated_pentagon", with_labels=False, node_size=300, node_color="black")
    return G


if __name__ == "__main__":
    # create_dilated_network()
    # create_square()
    # create_dilated_square()
    create_dilated_pentagon()

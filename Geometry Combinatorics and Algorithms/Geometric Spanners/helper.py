from math import sqrt
import matplotlib.pyplot as plt
import networkx as nx


def dist(p1, p2):
    x1, y1 = p1
    x2, y2 = p2
    return sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)


def create_figure(G, positions_dict, title, margin=0.1, node_size=1000, node_color="white", edge_color="black",
                  with_labels=True, font_size=10, nodelist=None, edgelist=None, save=True, show=True):
    plt.margins(x=margin)
    nodelist = nodelist if nodelist is not None else G.nodes()
    edgelist = edgelist if edgelist is not None else G.edges()
    nx.draw(G, pos=positions_dict, node_size=node_size, node_color=node_color, edge_color=edge_color,
            with_labels=with_labels, font_size=font_size, nodelist=nodelist, edgelist=edgelist)
    if save:
        plt.savefig(f"./images/{title}.pdf", format="pdf", bbox_inches='tight')
    if show:
        plt.show()

def calculate_coordinates_of_pentagon(dilate=False):
    c1 = (sqrt(5) - 1) / 4
    c2 = (sqrt(5) + 1) / 4
    s1 = sqrt(10 + 2 * sqrt(5)) / 4
    s2 = sqrt(10 - 2 * sqrt(5)) / 4
    pos_dict = {
        0: (0, 1),
        1: (s1, c1),
        2: (s2, -c2),
        3: (-s2, -c2),
        4: (-s1, c1)
    }
    if dilate:
        pos_dict[5] = (0, 0)
    return pos_dict
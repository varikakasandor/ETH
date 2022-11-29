from math import sqrt
import matplotlib.pyplot as plt
import networkx as nx


def dist(p1, p2):
    x1, y1 = p1
    x2, y2 = p2
    return sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)


def create_figure(G, positions_dict, title):
    plt.margins(x=0.1)
    nx.draw(G, pos=positions_dict, node_size=1000, node_color="white", edge_color="black", with_labels=True,
            font_size=10)
    plt.savefig(f"./images/{title}.pdf", format="pdf", bbox_inches='tight')
    plt.show()

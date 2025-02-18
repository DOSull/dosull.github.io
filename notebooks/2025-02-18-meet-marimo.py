import marimo

__generated_with = "0.11.0"
app = marimo.App()


@app.cell(hide_code=True)
def _(mo):
    n = mo.ui.slider(start=1, stop=1000, step=1, value=100, 
                     label='Number of points to show')
    n
    return (n,)


@app.cell(hide_code=True)
def _(n, plot_spaced_points, pp):
    plot_spaced_points(pp, n=n.value)
    return


@app.cell(hide_code=True)
def _(mo):
    mo.md(
        r"""
        The code to generate the interactive plot above is as follows. First, a plot function: 
        ```
        def plot_spaced_points(pp:pd.DataFrame, n:int):
            fig, ax = plt.subplots()
            ax.scatter(x=pp.x[:n], y=pp.y[:n])
            ax.set_aspect(1)
            ax.set_xlim(0, 1)
            ax.set_ylim(0, 1)
            return ax
        ```
        Then to make a slider control and display it:
        ```
        n = mo.ui.slider(start=1, stop=1000, step=1, value=100, 
                         label='Number of points to show')
        n
        ```
        And then a cell to make the plot based on the slider value:
        ```
        plot_spaced_points(pp, n=n.value)
        ```
        """
    )
    return


@app.cell(hide_code=True)
def _(math, pd, random):
    class RandomPoint():

        def __init__(self):
            self.x = random.random()
            self.y = random.random()

        def distance2(self, pt):
            dx = min(self.x - pt.x, 1 - self.x + pt.x)
            dy = min(self.y - pt.y, 1 - self.y + pt.y)
            return dx**2 + dy**2

    def spaced_points(n=100):
        pts = [RandomPoint()]
        for i in range(1, n):
            n_candidates = math.ceil(math.log(1.1 * i * math.e))
            p_new = [RandomPoint() for _ in range(n_candidates)]
            d_mins = [min([p.distance2(p0) for p0 in pts]) for p in p_new]
            i_max = max(enumerate(d_mins), key=lambda x: x[1])[0]
            pts.append(p_new[i_max])
        return pd.DataFrame(data = {'x': [p.x for p in pts], 
                                    'y': [p.y for p in pts]})
    return RandomPoint, spaced_points


@app.cell(hide_code=True)
def _(pd, plt, spaced_points):
    def plot_spaced_points(pp:pd.DataFrame, n:int):
        fig, ax = plt.subplots()
        ax.scatter(x=pp.x[:n], y=pp.y[:n])
        ax.set_aspect(1)
        ax.set_xlim(0, 1)
        ax.set_ylim(0, 1)
        return ax

    pp = spaced_points(1000)
    return plot_spaced_points, pp


@app.cell(hide_code=True)
def _():
    import random
    import math

    import matplotlib.pyplot as plt
    import pandas as pd
    return math, pd, plt, random


@app.cell(hide_code=True)
def _():
    import marimo as mo
    return (mo,)


if __name__ == "__main__":
    app.run()

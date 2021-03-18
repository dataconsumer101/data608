import dash
import dash_table
import dash_core_components as dcc
import dash_html_components as html
from dash.dependencies import Input, Output
import plotly.express as px
import pandas as pd

stylesheet = ['http://www.csszengarden.com/examples/style.css']
external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']

app = dash.Dash(__name__, external_stylesheets=external_stylesheets)



# loop until all records are pulled
def get_query(query):

    i = 0
    while True:

        o_query = query + '&$offset={}'.format(i * 1000).replace(' ', '%20')

        df = pd.read_json(o_query)

        if i == 0:
            df_all = df
        else:
            df_all = pd.concat([df_all, df])

        if df.shape[0] < 1000:
            break

        i += 1    

    return df_all


# For Species Drop Down
# get distinct list of tree species
query = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
    '$select=spc_common' +\
        '&$group=spc_common')

species = get_query(query)

# exclude null
species = species.dropna()


# For Borough Drop Down
query = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
    '$select=boroname' +\
        '&$group=boroname')

borough = get_query(query)

# exclude null
borough = borough.dropna()


# for Data Table
# get health
query = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
    '$select=spc_common,health,boroname,count(tree_id)' +\
        '&$group=spc_common,health,boroname')

df = get_query(query)

df_treesum = df.groupby(['spc_common', 'boroname']).sum().reset_index()
df_treesum.columns = ['spc_common', 'boroname', 'total']

df_health = df.merge(df_treesum, on=['spc_common','boroname'])
df_health['Share'] = round((df_health.count_tree_id / df_health.total) * 100, 1)
df_health['Share'] = df_health['Share'].astype('str') + ' %'

df_health = df_health[['spc_common', 'boroname', 'health', 'count_tree_id', 'Share']]
df_health.columns = ['spc_common', 'boroname', 'Health Condition', 'Count', 'Share']
df = []


# For Bar Graph
# get steward by health
query = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
    '$select=spc_common,health,steward,boroname,count(tree_id)' +\
        '&$group=spc_common,health,steward,boroname')

df = get_query(query)

df_stewsum = df.groupby(['spc_common', 'boroname', 'steward']).sum().reset_index()
df_stewsum.columns = ['spc_common', 'boroname', 'steward', 'total']

df_steward = df.merge(df_stewsum, on=['spc_common','boroname','steward'])
df_steward['share'] = round(df_steward.count_tree_id / df_steward.total, 4)
df = []

df_ph = pd.DataFrame(data={'Health Condition': [''], 'Count': 0, 'Share': [0]})
# fig = px.bar(x = ['steward'], y = [0])

app.layout = html.Div(children=[
    html.H3('Data608 Assignment 4 -- New York City Trees'),    
    html.H5('Tree Species (Common Name)'),
    dcc.Dropdown(
        id = 'dropdown',
        options = [{'label': i.title(), 'value': i} for i in species.spc_common],        
        value = 'sugar maple'
        # placeholder='Choose a tree species...'
    ),
    html.H5('NYC Borough'),
    dcc.Dropdown(
        id = 'location',
        options = [{'label': i.title(), 'value': i} for i in borough.boroname],        
        value = 'Manhattan'
        # placeholder='Choose a tree species...'
    ),
    html.Br(),    
    dash_table.DataTable(
        id = 'table',
        columns = [{'name': i, "id": i} for i in df_ph.columns]
    ),
    dcc.Graph(id = 'bar_graph')    
])


@app.callback(
    Output('table', 'data'),
    Output('bar_graph', 'figure'),    
    Input('dropdown', 'value'),
    Input('location', 'value'))
def update_figure(tree, loc):

    # data table
    df_dt = df_health[(df_health.spc_common == tree) & (df_health.boroname == loc)]
    df_dt = df_dt[['Health Condition', 'Count', 'Share']]

    # bar graph
    df_bg = df_steward[(df_steward.spc_common == tree) & (df_steward.boroname == loc)]
    fig = px.bar(df_bg, x = 'steward', y = 'share', color = 'health', title = 'Tree Health Percent Share by Steward Group')    
    fig.update_layout(
        xaxis_title = 'Steward Group',
        yaxis_title = 'Percent of Trees',
        legend_title = 'Health Condition'
    )

    return df_dt.to_dict('records'), fig



if __name__ == '__main__':
    app.run_server(debug=True)



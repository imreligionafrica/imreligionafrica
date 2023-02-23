import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

class CoefPlot(object):
    
    '''
    python wrapper for creating coefficient plots, similar to Stata's coefplot
    http://repec.sowi.unibe.ch/stata/coefplot/
    requires numpy, pandas, matplotlib
    
    Sebastian Hohmann, 2020
    '''
    
    def __init__(self, df):
        self.df = df
        
    def simple_dotplot(self, b, varname, err=None, figsize=(10,6),
                       sort_varname = False, sort_coefval = False,
                       zero_line=False):
        if sort_varname:
            df = self.df.sort_values(varname, inplace=False, ascending=False)
        elif sort_coefval:
            df = self.df.sort_values(b, inplace=False, ascending=True)
        elif sort_varname and sort_coefval:
            print('Error: sort by EITHER variable name OR coefficient value')
            df = self.df
        else:
            df = self.df
        
        f, ax = plt.subplots(1, 1, figsize=figsize)
        if err:
            ax.barh(y = df[varname], width = df[b],   
                    color='none', xerr=df[err])
        ax.scatter(x=df[b], y = df[varname])
        if zero_line:
            ax.axvline(x=0, linestyle='--', color='black', linewidth=1)
            
        ax.set_yticklabels(df[varname], fontdict={'fontsize': 15}, rotation=0, minor=False)
        ax.tick_params(axis='x', labelsize=15)
        
#         plt.show();

        return f
        
    def grouped_dotplot(self, b, varname, groupname, err=None, figsize=(10,6), 
                        order_groups_by_varlevel=None, order_groups_by_variable=None,
                        within_group_order=None, zero_line=False, 
                        group_separators=True, legend=True, legend_bbox=(0,-0.4),
                        legend_columns=1):

        if order_groups_by_varlevel:
            self.df['aux'] = self.df[self.df[varname]==order_groups_by_varlevel][b]
            self.df['aux'] = self.df[[groupname, 'aux']].groupby(groupname).transform(max)
            self.df.sort_values(['aux'], inplace=True, ascending=False)
            del self.df['aux']
        elif order_groups_by_variable:
            self.df.sort_values([order_groups_by_variable], ascending=True, inplace=True)
        group_order = list(pd.unique(self.df[groupname]))
        
        if not within_group_order:
            within_group_order = list(set(self.df[varname]))
        
        offsets = self.make_offset_grid(len(within_group_order))
#         offsets = self.make_offset_grid2(len(within_group_order))
        markseq = 'sox^vD+p' # can extend this https://matplotlib.org/3.1.1/api/markers_api.html
        colseq = 'bgrcmykw' # can extend this https://matplotlib.org/3.1.0/api/colors_api.html
    
        f, ax = plt.subplots(1, 1, figsize=figsize)
        
        ymin = np.floor(np.min(offsets))
        for ig, g in enumerate(reversed(group_order)):
            X = (2*ig)*np.ones(len(offsets)) + offsets
            for iv, v in enumerate(reversed(within_group_order)):
                if err:
                    ax.barh(y=X[iv], width=self.df[(self.df[groupname]==g) & 
                                                   (self.df[varname]==v)][b],
                            color='none', xerr=self.df[(self.df[groupname]==g) & 
                                                       (self.df[varname]==v)][err])
                lab = v if ig == 0 else None
                ax.scatter(x=self.df[(self.df[groupname]==g) & 
                                     (self.df[varname]==v)][b],
                           y=X[iv], marker=markseq[iv], color=colseq[iv],
                           label=lab)
        ymax = np.ceil(np.max(X))
                
        ax.set_yticks(np.arange(0, len(group_order)*2, 2), minor=False)
        ax.set_yticklabels(reversed(group_order), fontdict={'fontsize': 15}, rotation=0, minor=False)
        ax.yaxis.grid(False, which='major')
        ax.tick_params(axis='x', labelsize=15)
        ax.set_ylim([ymin, ymax])
        
        # tweak legend
        handles, labels = ax.get_legend_handles_labels()
        if legend:
            if within_group_order:
                newhandles = ()
                newlabels = ()            
                for lab in within_group_order:
                    for il, label in enumerate(labels):
                        if lab == label:
                            newhandles = newhandles + (handles[il],)
                            newlabels = newlabels + (label,)   
                ax.legend(newhandles, newlabels, loc='lower left',
                bbox_to_anchor=legend_bbox, ncol=legend_columns, frameon=True, 
                fontsize=15)
            else:
                ax.legend(reversed(handles), reversed(labels), loc='lower left',
                          bbox_to_anchor=legend_bbox, ncol=legend_columns, frameon=True, 
                          fontsize=15)

        if zero_line:
            ax.axvline(x=0, linestyle='--', color='black', linewidth=1)
            
        # change color if have white figure background
        if group_separators:
            for y in np.arange(1, (len(group_order)-1)*2,2):
                ax.axhline(y=y, linestyle='-', color='w', linewidth=1)

#         plt.show();

        return f
    
    def make_offset_grid(self, npoints):
        grid = np.linspace(-0.25*(npoints//2), 0.25*(npoints//2), npoints)
        return grid 
    
    def make_offset_grid2(self, npoints):
        grid = np.linspace(-0.2*(npoints//2), 0.2*(npoints//2), npoints)
        return grid 
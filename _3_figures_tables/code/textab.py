class TexTab(object):
    def __init__(self, df):
        self.df = df
           
    def create_header(self, cns, gaps, lcols, cap, lab,
                      rzbx=True, colnums=True, clines=False):
        
        # first part: everything from "\begin{table}"
        # until the "\hline" underneath "\begin{tabular}"
        
        l1 = "\\begin{table}[ht!]\n\\singlespacing\n"
        l2 = "\\centering\n"
        l3 = "\\caption{%s}\n" % cap
        l4 = "\\label{%s}\n" % lab
        if rzbx == True:
            l5 = "\\resizebox{\\columnwidth}{!}{\n"
        else:
            l5 = ""
        
        col_align = ""
        for v in self.df.columns:
            if v in lcols:
                col_align += "l"
            else:
                col_align += "c"
            if v in gaps:
                col_align += "c"
        
        l6 = "\\begin{tabular}{%s}\n" % col_align 
        l7 = "\\hline\n"
        
        # second part: column numbers and column names
        
        l8 = ""
        l9 = ""
        iv = 1
        for vx, v in enumerate(self.df.columns):
            vn = cns[vx]
            if 'float' in str(self.df[v].dtypes) or 'int' in str(self.df[v].dtypes):
                if vx != len(self.df.columns)-1:
                    l8+="(%s)&" % iv
                    iv+=1
                else:
                    l8+="(%s)" % iv
            else:
                if vx != len(self.df.columns)-1:
                    l8+="&"
            if vx != len(self.df.columns)-1:        
                l9+= "%s&" % vn
            else:
                l9+= "%s" % vn
            if v in gaps:
                l8+="&"
                l9+="\;\;\;\;\;\;\;\;\;\;\;&"
        
        # third part: adding clines (if want them)
        
        cline = ""
        i0 = 1
        igap=0
        for iv, vn in enumerate(cns):
            i1=iv+igap+1
            cl = "\\cline{%s-%s}" % (i0, i1)
            if vn in gaps:
                cline+=cl
                i0=i1+2
                igap+=1
        cline+="\\cline{%s-%s}" % (i0, i1)
    
        l9+="\\\\\n"
        
        if clines==True:
            l8+="\\\\\n%s\n" % cline
            l10 = "%s\n" % cline
        else:
            l8+="\\\\\n\\hline\n"
            l10 = "\\hline\n"
            
        # putting everything together

        header = l1+l2+l3+l4+l5+l6+l7+l8+l9+l10
        
        return(header, cline)
    
    def create_data(self, lines, clines, cns, gaps, cline): 
        
        dat = ""
        for i in range(self.df.shape[0]):

            if self.df.iloc[i,0] in lines:
                if clines == True:
                    dat+=cline+"\n"
                else:
                    dat+="\\hline\n"
            
            l = ""
            for vx, v in enumerate(self.df.columns):
                vn = cns[vx]
                if vx != len(self.df.columns)-1:
                    if "float" in str(type(self.df.iloc[i,vx])):
                        l+="%s&" % round(self.df.iloc[i,vx], 3)
                    elif "int" in str(type(self.df.iloc[i,vx])):
                        intval = self.df.iloc[i,vx]
                        fint = f'{intval:,}' 
                        l+="%s&" % fint
                    else:
                        l+="%s&" % self.df.iloc[i,vx]
                else:
                    if "float" in str(type(self.df.iloc[i,vx])):
                        l+="%s" % round(self.df.iloc[i,vx], 3)
                    elif "int" in str(type(self.df.iloc[i,vx])):
                        intval = self.df.iloc[i,vx]
                        fint = f'{intval:,}' 
                        l+="%s" % fint                        
                    else:
                        l+="%s" % self.df.iloc[i,vx]
                if v in gaps:
                    l+="&"
            l+="\\\\\n"
            dat+=l
            
        return(dat)
        
    def create_footer(self, rzbx, notes):
        l1 = "\\hline\n"
        l2 = "\\end{tabular}\n"
        if rzbx == True:
            l3 = "}\n"
        else:
            l3 = ""
        l4 = "\\captionsetup{size=scriptsize, justification=justified, width=\\columnwidth}\n"
        l5 = "\\caption*{%s}\n" % notes
        l6 = "\\end{table}"
            
        footer=l1+l2+l3+l4+l5+l6
    
        return(footer)
    
        
    def create_tab_onepanel(self, cns, gaps, lcols, cap, lab, lines, notes,
                            rzbx=True, colnums=True, clines=False):
                
        # 1) creating the header
        
        head, cline = self.create_header(cns, gaps, lcols, cap, lab,
                                        rzbx, colnums, clines)
        tab = head
        
        # 2) adding the data
        
        dat = self.create_data(lines, clines, cns, gaps, cline)
        tab+=dat

        # 3) adding the footer

        foot = self.create_footer(rzbx, notes)
        
        tab+=foot

        return(tab)
        
    def create_tab_multpanel(self, cns, gaps, lcols, cap, lab, lines, notes, panelvar, panelrules,
                             rzbx=True, colnums=True, clines=False):
        
        pass
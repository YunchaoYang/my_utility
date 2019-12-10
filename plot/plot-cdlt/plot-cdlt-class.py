import numpy as np
import matplotlib.pyplot as plt

class WriteFigure(object):


    
    def __init__(self, fname, column, vname):
        """Return a Customer object whose name is *name*.""" 
        self.fname = fname
        self.column = column
        self.column = self.column - 1
        self.vname = vname
        self.Step_all = []
        self.V_all = []

        try:
            with open(self.fname,'r') as fl:
                line1 = fl.readline()
                lines = fl.readlines()
            for line in lines:
                step = float(line.split()[0])
                varia = float(line.split()[self.column])
                self.Step_all.append(step)
                self.V_all.append(varia)
            
        except:
            print("Cannot open file name")
 
        self.writepng()
    
    def writepng(self, yaxis_name = None):
        # create a new figure
        plt.figure()
# plot the point (3,2) using the circle marker
        fig, = plt.plot(self.Step_all, self.V_all)
        
        variable_number = len(self.V_all)
        
        second_half = int(len(self.V_all) / 2)
        
        V_avg = sum(self.V_all[second_half:]) / float(second_half)
        
        textstr = '$\mu=%.2f$\n$\mathrm{median}=%.2f$\n$\sigma=%.4f$'%(mu, median, sigma)
        textstr = 'Mean result: %.4f'%(V_avg)
    
# get the current axes
        ax = plt.gca()
# Set axis properties [xmin, xmax, ymin, ymax]
#    ax.axis([0,6,0,10])
        ax.set_xlabel('Iterations')        
        if yaxis_name != None:
            ax.set_ylabel(yaxis_name)
        else:
           ax.set_ylabel(self.vname) 

        ax.set_title('Lift coefficient vs. iterations')
 
    # these are matplotlib.patch.Patch properties
        props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)

# place a text box in upper left in axes coords
        ax.text(0.5, 0.15, textstr, transform=ax.transAxes, fontsize=14,
        verticalalignment='top', bbox=props)

        name = self.vname + '-'+ str(int(self.Step_all[0])) + '-' + str(int(self.Step_all[-1])) + '.png'
        print(name)
        plt.savefig(name)
#    plt.close(fig)

        


plot1 = WriteFigure('cdlt.his', 2, 'CL')

plot2 = WriteFigure('cdlt.his', 3, 'CD')

plot3 = WriteFigure('jet.his', 4, 'Cmu')

plot4 = WriteFigure('jet.his', 5, 'Pc')




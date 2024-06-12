import json
import numpy as np
from matplotlib import pyplot as plt, colors

class ARCTask():
  _cmap = colors.ListedColormap(['#2B2B2B', '#248ADA','#C71010','#1FC719','#F7DE28', '#878494', '#F954F2', '#EE6000', '#6B23A9', '#8B5A28'])
  _norm = colors.Normalize(vmin = 0, vmax = 9)
  
  def __init__(self, filename):
    with open(filename) as file:
      self._filename = filename
      self._data = json.load(file)

      self.train = [np.stack([np.array(pair['input']), np.array(pair['output'])]) for pair in self._data['train']]
      self.test = [np.stack([np.array(pair['input']), np.array(pair['output'])]) for pair in self._data['test']]
      
  def show(self):
    fig, axs = plt.subplots(2, len(self.train) + len(self.test), figsize=(4,2), dpi=600, layout='constrained')
    
    for i, pair in enumerate(self.train):
      axs[0,i].set_title('Example %d' % (i + 1))
      axs[0,i].imshow(pair[0], cmap=self._cmap, norm=self._norm)
      axs[1,i].imshow(pair[1], cmap=self._cmap, norm=self._norm)
    
    for i, pair in enumerate(self.test):
      axs[0,i+len(self.train)].set_title('Test %d' % (i + 1))
      axs[0,i+len(self.train)].imshow(pair[0], cmap=self._cmap, norm=self._norm)
      axs[1,i+len(self.train)].imshow(pair[1], cmap=self._cmap, norm=self._norm)
      
    for ax in axs.ravel():
        ax.set_axis_off()

    plt.show()
    return fig
  
  def __repr__(self):
    return('<ARCTask "", loaded from "%s">' % self._filename)

import random
import numpy
from matplotlib import pyplot

x = [random.gauss(3,1) for _ in range(400)]
y = [random.gauss(4,2) for _ in range(400)]
z = [random.gauss(5,1) for _ in range(400)]


bins = numpy.linspace(-10, 10, 100)

pyplot.hist(x, bins, alpha=0.5, label='x')
pyplot.hist(y, bins, alpha=0.5, label='y')
pyplot.hist(z, bins, alpha=0.5, label='z')

pyplot.legend(loc='upper right')
pyplot.show()
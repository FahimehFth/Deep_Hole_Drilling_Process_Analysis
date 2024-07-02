# Deep_Hole_Drilling_Process_Analysis
An in-depth study on the BTA (Boring and Trepanning Association) deep hole drilling process.

One of the most important applications of drilling is to produce extremely precise and
smooth holes, such as axial bores in turbines or compressor shafts. However, drilling is
a complex and dynamic process that can be affected by various disturbances, such as
chattering and spiralling. Chattering is a self-excited vibration that occurs when the
drill bit interacts with the workpiece material, resulting in irregular cutting forces and
poor surface quality. Spiralling is a deviation of the drill bit from its intended path,
resulting in a helical-shaped hole that does not match the desired geometry. Both of
these phenomena can compromise the performance and safety of the drilled components,
and cause damage to the tool or the machine. Therefore, it is important to analyze the
machine’s behavior and detect the problem earlier, before it leads to catastrophic failure
or costly rework.

This project utilizes data from 10 experiments that are part of Project C5 and are measured
by the Institut für Spanende Fertigung, FB Maschinenbau, University of Dortmund.
This data contains 7 sensors to measure different parameters of the machining
process. The purpose of the analysis is to select the suitable sensor in order to provide
an early warning before chattering or spiralling occurs.
The experiments data is provided in two types of files: header files and data files. The
header files are text files that contained information about the recording conditions. The
data files are binary files that stored the A-to-D converted data from each recording,
obtained through the GX-1 device. The first step of the analysis is to read the data
files in a readable format, using the header files as a reference. Then, time series plots
are generated to visualize the patterns of each sensor in an experiment and to identify
changes.

A custom function is developed that analyzes the pattern changes in the "Moment"
sensor and the dominant frequencies in the periodograms. The function compares the
values of these features with a predefined threshold and flags the phase that exceeds it.
This way, it can detect the problematic phases.

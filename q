# Bash script for compilation and running of the Thornley Transport Resistance Model
#----------------------------------------------------------------------------------------#
#set -e

var1 = "$1"
echo $var1

# Delete previous object file
#----------------------------------------------------------------------------------------#
if [ -e tmp/uptake.txt ] ; then rm tmp/*.txt ; fi
if [ -e fig/uptake.png ] ; then rm fig/*.png ; fi
if [ -e TTR.o     ] ; then rm TTR.o     ; fi

# Compile the executable
#----------------------------------------------------------------------------------------#
gfortran -c TTR_parameters.f90
gfortran -c TTR_variables.f90
gfortran -c TTR_C_uptake.f90
gfortran -c TTR_N_uptake.f90
gfortran -c TTR_growth.f90
gfortran -c TTR_loss.f90
gfortran -c TTR_growth.f90
gfortran -c TTR_utilisation.f90
gfortran -c TTR_respiration.f90
gfortran -c TTR_transport.f90
gfortran -c TTR_increments.f90
gfortran -c TTR_model.f90
gfortran -o TTR_model TTR_parameters.o TTR_variables.o TTR_C_uptake.o TTR_N_uptake.o TTR_growth.o TTR_loss.o TTR_respiration.o TTR_transport.o TTR_utilisation.o TTR_increments.o TTR_model.o

# Run the executable
#----------------------------------------------------------------------------------------#
./TTR_model
cp driver.txt tmp

echo "Simulation done!"

# Delete the executable file 
#----------------------------------------------------------------------------------------#
rm TTR_model
rm *.o

# Open R script to plot outputs if outputs are desired
#----------------------------------------------------------------------------------------#
if [ $var1 == 'output']
then
  R CMD BATCH plot_pools.R
  R CMD BATCH plot_uptake.R                          
  R CMD BATCH plot_growth.R
  R CMD BATCH plot_loss.R
  R CMD BATCH plot_transport.R
  R CMD BATCH plot_increments.R
  R CMD BATCH plot_concentrations.R

  # Stitch images together the image files
  #--------------------------------------------------------------------------------------#
  cd fig/
  convert -append plant_C_utilisation.png                                                  \
                  plant_C_transport.png                                                    \
                  plant_C_concentrations.png                                               \
                  plant_C_pools.png                                                        \
                  C_increments.png                                                         \
                  combined1.png
                 
  convert -append plant_N_utilisation.png                                                  \
                  plant_N_transport.png                                                    \
                  plant_N_concentrations.png                                               \
                  plant_N_pools.png                                                        \
                  N_increments.png                                                         \
                  combined2.png
                
  convert -append uptake.png                                                               \
                  plant_growth.png                                                         \
                  plant_X_growth.png                                                       \
                  plant_M_growth.png                                                       \
                  plant_loss.png                                                           \
                  combined3.png
                
  convert -append plant_total_respiration.png                                              \
                  plant_X_pools.png                                                        \
                  X_increments.png                                                         \
                  plant_M_pools.png                                                        \
                  M_increments.png                                                         \
                  combined4.png
                
  convert -append leaf_increments.png                                                      \
                  branch_increments.png                                                    \
                  stem_increments.png                                                      \
                  coarse_root_increments.png                                               \
                  fine_root_increments.png                                                 \
                  combined5.png
                
  convert +append combined1.png                                                            \
                  combined2.png                                                            \
                  combined3.png                                                            \
                  combined4.png                                                            \
                  combined5.png                                                            \
                  combined.png
  rm combined?.png

  # Move back to TTR_model directory
  #--------------------------------------------------------------------------------------#
  cd ..
fi

echo 'Done!'

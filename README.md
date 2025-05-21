# mthm503

This is a template for course MTHM503 "Applications of Data Science and Statistics".  You should fork this repository and use it for your work on this module

Note: in a professional setting, a DevOps engineer (or team of engineers) would be responsible for setting up infrastructure like this. You only have to operate within the infrastructure.  Therefore, you may find yourself working as follows:

1. Develop a script to perform some analysis task.
2. When you are happy with it, wrap it in a function and save it in the functions.R file.
3. You call this function from the _targets.R file in the form 
  tar_target(NAME OF THE OUTPUT, FUNCTION CALL(DATA AND OTHER ARGUMENTS))
4. Run tar_make() in the parent folder. 




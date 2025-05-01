# (Optional) If the Local Branch Was Also Deleted
#If git branch does not list In_development, you can recreate it from the last
#commit before deletion. Based on your reflog, the last commit in In_development
#is 8a09e5d. To restore it, run:

# git checkout -b In_development 8a09e5d
# git push origin In_development

#This will recreate the branch locally and push it back to GitHub.

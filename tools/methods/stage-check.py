"""Build an isolated review candidate with proposed shared metadata additions."""
from pathlib import Path
import shutil, subprocess, sys, tempfile
root=Path.cwd(); run=Path(sys.argv[1]).resolve()
stage=Path(tempfile.mkdtemp(prefix='infer-methods-'))
package=stage/'infercalcR'; shutil.copytree(root,package,ignore=shutil.ignore_patterns('.git','*.tar.gz','*.Rcheck'))
p=package/'DESCRIPTION';s=p.read_text();s=s.replace('Suggests: digest, jsonlite','Suggests: digest, jsonlite, cluster, nlme, MASS, nnet, lme4, geepack');p.write_text(s)
p=package/'NAMESPACE';p.write_text(p.read_text()+'\nexport(parallel_analysis)\nexport(predict_inference)\nS3method(as_inference,htest)\nS3method(as_inference,lm)\nS3method(as_inference,glm)\n')
library=run/'libraries/methods';library.mkdir(parents=True,exist_ok=True)
for command in [['R','CMD','build','--no-build-vignettes',str(package)]]:
 subprocess.run(command,cwd=stage,check=True)
tarball=next(stage.glob('infercalcR_*.tar.gz'))
subprocess.run(['R','CMD','INSTALL','--install-tests','--library='+str(library),str(tarball)],check=True)
(run/'handoffs/P05-staged-path.txt').write_text(str(stage)+'\n')
shutil.copy2(tarball,run/'P05-methods-staged.tar.gz')
print(stage)

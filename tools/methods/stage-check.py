"""Build an isolated review candidate with proposed shared metadata additions."""
from pathlib import Path
import re, shutil, subprocess, sys, tempfile
root=Path.cwd(); run=Path(sys.argv[1]).resolve()
stage=Path(tempfile.mkdtemp(prefix='infer-methods-'))
package=stage/'infercalcR'
provider=root.parent/'infercalcR'
shutil.copytree(provider,package,ignore=shutil.ignore_patterns('.git','*.tar.gz','*.Rcheck'))
patterns=['R/method-spec.R','R/as-inference.R','R/observed-*.R','man/observed*.Rd','man/as_inference.Rd','tests/observed*.R','tests/as-inference.R','inst/methods/*','tools/methods/*']
for pattern in patterns:
 for source in root.glob(pattern):
  if source.is_file():
   destination=package/source.relative_to(root);destination.parent.mkdir(parents=True,exist_ok=True);shutil.copy2(source,destination)
p=package/'DESCRIPTION';s=p.read_text()
required=['cluster','nlme','MASS','nnet','lme4','geepack']
match=re.search(r'^Suggests: (.*)$',s,re.M);entries=[x.strip() for x in match.group(1).split(',')]
for name in required:
 if name not in entries:entries.append(name)
s=s[:match.start()]+('Suggests: '+', '.join(entries))+s[match.end():];p.write_text(s)
p=package/'NAMESPACE';s=p.read_text()
for line in ['export(parallel_analysis)','export(predict_inference)','S3method(as_inference,htest)','S3method(as_inference,lm)','S3method(as_inference,glm)']:
 if line not in s.splitlines():s+='\n'+line
p.write_text(s+'\n')
library=run/'libraries/methods';library.mkdir(parents=True,exist_ok=True)
for command in [['R','CMD','build','--no-build-vignettes',str(package)]]:
 subprocess.run(command,cwd=stage,check=True)
tarball=next(stage.glob('infercalcR_*.tar.gz'))
if '--no-install' not in sys.argv[2:]:
 subprocess.run(['R','CMD','INSTALL','--install-tests','--library='+str(library),str(tarball)],check=True)
(run/'handoffs/P05-staged-path.txt').write_text(str(stage)+'\n')
shutil.copy2(tarball,run/'P05-methods-staged.tar.gz')
print(stage)

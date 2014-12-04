printf('lendo problema %s ...\n', problema);

n_entradas= 6; n_clases= 4; n_fich= 1; fich{1}= 'car.data'; n_patrons(1)= 1728; %fich{2}= ' '; n_patrons(2)= 0;

n_max= max(n_patrons);
x = zeros(n_fich, n_max, n_entradas); cl= zeros(n_fich, n_max);

n_patrons_total = sum(n_patrons); n_iter=0;
clase = {'unacc', 'acc', 'good', 'v-good'};

for i_fich=1:n_fich
  f=fopen(fich{i_fich}, 'r');
  if -1==f
	error('erro en fopen abrindo %s\n', fich{i_fich});
  end
  for i=1:n_patrons(i_fich)
  	fprintf(2,'%5.1f%%\r', 100*n_iter++/n_patrons_total);
	for j = 1:n_entradas
	  t= fscanf(f,'%s',1);
	  if j==1 || j==2
		val={'vhigh', 'high', 'med', 'low'};
	  elseif j==3
		val={'2', '3', '4', '5-more'};
	  elseif j==4
		val={'2', '4', 'more'};
	  elseif j==5
		val={'small', 'med', 'big'};
	  elseif j==6
		val={'low', 'med', 'high'};
	  end
	  n=length(val); a=2/(n-1); b=(1+n)/(1-n);
	  for k=1:n
		if strcmp(t,val{k})
		  x(i_fich,i,j)=a*k+b; break
		end
	  end
	end
	t = fscanf(f,'%s',1);   % lectura da clase
	for j=1:n_clases
	  if strcmp(t,clase{j})
		cl(i_fich,i)=j; break
	  end
	end
  end
  fclose(f);
end

max_n = 10;
n = 0:max_n;
cantor = [[] for i in n];
cantor[1] = [[0, 1]];

for i in 1:max_n
	# N = 2^i -1 #normalization
	cantor[i + 1] = deepcopy(cantor[i])
	for j in 1:length(cantor[i])
		app_cantor = [sum([cantor[i + 1][j], [2^i, 2^i]])]
		cantor[i + 1] = append!(cantor[i + 1], app_cantor)
	end
end

for i in 1:length(cantor)
	cantor[i] = cantor[i] / maximum(maximum(cantor[i]))
end

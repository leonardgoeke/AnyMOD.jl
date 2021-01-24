

# single

bla = getAllVariables(:emission,anyM)

@time begin
	add_to_expression!(bla[1,:var], bla[2,:var])
end

bla = getAllVariables(:emission,anyM)

@time begin
	bla[1,:var] = bla[1,:var] + bla[2,:var]
end

bla = getAllVariables(:emission,anyM)

@time begin
	bla[1,:var] + bla[2,:var]
end


# all


bla = getAllVariables(:emission,anyM)

@time begin
    b = AffExpr()
	map(x -> add_to_expression!(b,x), bla[!,:var])
end

bla = getAllVariables(:emission,anyM)

@time begin
	b = sum(bla[!,:var])
end

bla = getAllVariables(:emission,anyM)

@time begin
    b  = @expression(anyM.optModel, sum(bla[!,:var]))
end




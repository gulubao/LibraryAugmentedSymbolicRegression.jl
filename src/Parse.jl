module ParseModule

using DispatchDoctor: @unstable
using DynamicExpressions
using DynamicExpressions.NodeModule: Node
using SymbolicRegression: AbstractOptions, DATA_TYPE

"""
    parse_expr(expr_str::String, options)

Given a string (e.g., from string_tree) and an options object (containing
operators, variable naming conventions, etc.), reconstruct an
AbstractExpressionNode.
"""
@unstable function parse_expr(
    ::Type{T}, expr_str::String, options::AbstractOptions
)::AbstractExpression{T} where {T<:DATA_TYPE}
    node_type = options.node_type{T}::Type{<:AbstractExpressionNode{T}}
    expression_type = options.expression_type{T,node_type}::Type{<:AbstractExpression{T}}
    ops = options.operators
    varnames = get_variable_names(options.variable_names)
    expr_str_norm = _normalize_expr_string(expr_str)

    local ast
    try
        ast = Meta.parse(expr_str_norm)
    catch e
        if occursin(r"^\s*[^=\n]+=", expr_str_norm)
            stripped = replace(expr_str_norm, r"^\s*[^=\n]+=\s*" => "")
            try
                ast = Meta.parse(stripped)
            catch
                @warn "Failed to Meta.parse even after stripping LHS: $expr_str"
                @warn "Error: $e"
                @warn "Returning a constant node with value 1."
                return Expression(
                    node_type(; val=convert(T, 1.0));
                    options.operators,
                    options.variable_names,
                )
            end
        else
            @warn "Failed to Meta.parse: $expr_str"
            @warn "Error: $e"
            @warn "Returning a constant node with value 1."
            return Expression(
                node_type(; val=convert(T, 1.0)); options.operators, options.variable_names
            )
        end
    end

    ast = _rhs_of_assignment(ast)
    ast = _fix_unary_minus(ast)

    try
        return parse_expression(
            ast;
            operators=ops,
            node_type=node_type,
            expression_type=expression_type,
            variable_names=varnames,
        )::AbstractExpression{T}
    catch e
        # Try to recover from missing variable error
        msg = sprint(showerror, e)
        m = match(r"Variable `([^`]+)` not found", msg)
        if m !== nothing
            missing_var = m.captures[1]
            closest = find_closest_match(missing_var, varnames)
            if closest !== nothing
                @warn "Variable `$missing_var` not found. Correcting to `$closest`."
                # Replace with word boundaries to avoid partial replacements
                # We assume missing_var is a valid identifier, so \b works.
                new_expr = replace(expr_str_norm, Regex("\\b" * missing_var * "\\b") => closest)
                
                if new_expr != expr_str_norm
                    try
                        # Re-parse the corrected string
                        new_ast = Meta.parse(new_expr)
                        new_ast = _rhs_of_assignment(new_ast)
                        
                        return parse_expression(
                            new_ast;
                            operators=ops,
                            node_type=node_type,
                            expression_type=expression_type,
                            variable_names=varnames,
                        )::AbstractExpression{T}
                    catch e2
                        @warn "Failed to parse even after correction: $new_expr"
                        @warn "Error: $e2"
                    end
                end
            end
        end

        @warn "Failed to parse expression: $expr_str"
        @warn "Normalized: $expr_str_norm"
        @warn "Error: $e"
        @warn "Returning a constant node with value 1."
        return Expression(
            node_type(; val=convert(T, 1.0)); options.operators, options.variable_names
        )
    end
end

@unstable function _rhs_of_assignment(ast::Expr)
    if ast isa Expr
        if ast.head === :(=) && length(ast.args) ≥ 2
            return ast.args[2]
        elseif ast.head === :block && !isempty(ast.args)
            return _rhs_of_assignment(last(ast.args))
        end
    end
    return ast
end

@unstable function _rhs_of_assignment(ast::Symbol)
    return ast
end

function _fix_unary_minus(expr::Expr)
    if expr.head == :call && expr.args[1] == :- && length(expr.args) == 2
        # It's a unary minus: -(x) -> neg(x)
        return Expr(:call, :neg, _fix_unary_minus(expr.args[2]))
    else
        # Recurse on args
        return Expr(expr.head, map(_fix_unary_minus, expr.args)...)
    end
end

function _fix_unary_minus(x)
    return x
end

function _normalize_expr_string(s::AbstractString)
    # Normalize whitespace
    s = replace(s, r"\s+" => " ")
    # Replace standalone C or (C) with 1.0 or (1.0)
    s = replace(s, r"(?<!\w)C(?!\w)" => "1.0")
    s = replace(s, r"(?<!\w)\(C\)(?!\w)" => "(1.0)")
    # TODO: Right now, making all variables lowercase. This might not always be desired.
    s = lowercase(s)
    s = replace(s, r"\*\*" => "^")
    strip(s)
end

"""
    render_expr(ex::AbstractExpression{T}, options::AbstractOptions) -> String

Given an AbstractExpression and an options object, return a string representation
of the expression. Specifically, replace constants with "C" and variables with
"x", "y", "z", etc or the prespecified variable names.
"""
function render_expr(
    ex::AbstractExpression{T}, options::AbstractOptions
)::String where {T<:DATA_TYPE}
    return render_expr(get_contents(ex), options)
end

@unstable function _sketch_const(val)
    does_not_need_brackets = (typeof(val) <: Union{Real,AbstractArray})

    if does_not_need_brackets
        if isinteger(val) && (abs(val) < 5) # don't abstract integer constants from -4 to 4, useful for exponents
            string(val)
        else
            "C"
        end
    else
        if isinteger(val) && (abs(val) < 5) # don't abstract integer constants from -4 to 4, useful for exponents
            "(" * string(val) * ")"
        else
            "(C)"
        end
    end
end

function render_expr(tree::AbstractExpressionNode{T}, options)::String where {T<:DATA_TYPE}
    variable_names = get_variable_names(options.variable_names)
    return string_tree(
        tree, options.operators; f_constant=_sketch_const, variable_names=variable_names
    )
end

function get_variable_names(variable_names::Dict)::Vector{String}
    return [variable_names[key] for key in sort(collect(keys(variable_names)))]
end

function get_variable_names(variable_names::Nothing)::Vector{String}
    return ["x", "y", "z", "k", "j", "l", "m", "n", "p", "a", "b"]
end

function levenshtein(s1, s2)
    m, n = length(s1), length(s2)
    d = Matrix{Int}(undef, m+1, n+1)
    for i in 0:m; d[i+1, 1] = i; end
    for j in 0:n; d[1, j+1] = j; end
    for i in 1:m, j in 1:n
        cost = s1[i] == s2[j] ? 0 : 1
        d[i+1, j+1] = min(d[i, j+1]+1, d[i+1, j]+1, d[i, j]+cost)
    end
    return d[m+1, n+1]
end

function find_closest_match(target::String, candidates::Vector{String})
    best_match = nothing
    min_dist = typemax(Int)
    for cand in candidates
        dist = levenshtein(target, cand)
        if dist < min_dist
            min_dist = dist
            best_match = cand
        end
    end
    # Heuristic: only accept if distance is reasonable
    # Allow a bit more flexibility for longer words
    threshold = max(3, length(target) ÷ 2)
    if min_dist <= threshold
        return best_match
    end
    return nothing
end

end

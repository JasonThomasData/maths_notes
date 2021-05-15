function testThrowsError(f, errorParams)
    % given a function handle, f, and params that fail (cell array), make sure the function returns an error
    % This is an attempt to make a generic test function for future use
    if ~isa(f, 'function_handle')
        error("Pass a function handle to test eg: testThrowsError(@f, {1,2,3})");
    elseif ~isa(errorParams, 'cell')
        error("Pass a cell array of values, to pass to @f eg: testThrowsError(@f, {1,2,3})");
    elseif length(errorParams) > 3
        error("More than three parameters unsupported");
    end

    paramsCount = length(errorParams);
    % TODO: is there a dynamic way to enter an unknown number of
    % params into a function? That would be much better than this
    try
        if paramsCount == 1
            f(errorParams{1});
        elseif paramsCount == 2
            f(errorParams{1}, errorParams{2});
        elseif paramsCount == 3
            f(errorParams{1}, errorParams{2}, errorParams{3});
        end
        fprintf("FAIL: %s(params) didn't throw error with params: ", func2str(f));
        disp(errorParams);
    catch
        fprintf("PASS: %s(params) did throw error with params: ", func2str(f));
        disp(errorParams);
    end

end
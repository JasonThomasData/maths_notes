function [value, grad]  = Neg_Areas(rx)
    switch nargout
        case 1
            % change sign to maximise
            value = -(rx(1) + rx(2) + rx(3));
        case 2
            value = -(rx(1) + rx(2) + rx(3));
            grad = [-1; -1; -1; 0; 0; 0; 0; 0; 0];
    end
end

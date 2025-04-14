def to_shape_scale(k_dispersion, R0):
    
    # REASONS FROM HERE - https://github.com/reconhub/epitrix/blob/master/R/gamma_tools.R

    shape = []
    scale = []
    for k in k_dispersion:
        if k == "inf":
            k = 10**7
        shape.append(k)
        scale.append(R0/k)

    results = {}
    for i, k, in enumerate(k_dispersion):
        results[str(k)] = {
            "shape": shape[i],
            "scale": scale[i]
        }

    return results

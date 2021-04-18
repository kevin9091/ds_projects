import pandas as pd
from sklearn import model_selection

if __name__ == "__main__":
    df = pd.read_csv("../../../data/winequality-red.csv",sep=";")
    df["kfold"] = -1
    df = df.sample(frac=1).reset_index(drop=True)
    y = df.quality.values
    
    kf = model_selection.StratifiedKFold(n_splits=5)
    for f, (t_, v_) in enumerate(kf.split(X=df, y=y)):
        df.loc[v_, 'kfold'] = f
    
    df.to_csv("train_folds.csv", index=False)
from flask import Flask, request, jsonify, flash, redirect, url_for, send_from_directory
import joblib
import traceback
import pandas as pd
import numpy as np

UPLOAD_FOLDER = 'uploads'
ALLOWED_EXTENSIONS = {'txt', 'csv'}

app = Flask(__name__)
app.config['UPLOAD_FOLDER'] = UPLOAD_FOLDER

def allowed_file(filename):
    return '.' in filename and \
        filename.rsplit('.', 1)[1].lower() in ALLOWED_EXTENSIONS


@app.route('/predict', methods=['POST','GET'])
def predict():
    if lr:
        if request.method == 'POST':
            if 'file' not in request.files:
                flash('No file part')
                return redirect(request.url)
            file = request.files['file']
            if file.filename == '':
                flash('No selected file')
                return redirect(request.url)
            if file and allowed_file(file.filename):
                query = pd.get_dummies(pd.DataFrame(file))
                query = query.reindex(columns=model_columns, fill_value=0)
                df = pd.DataFrame(lr.predict(query), columns = ['pred'])
                df.to_csv('uploads/pred.csv')
                return redirect(url_for('download_file', name='pred.csv'))
        return '''
        <!doctype html>
        <title>Upload new File</title>
        <h1>Upload new File</h1>
        <form method=post enctype=multipart/form-data>
        <input type=file name=file>
        <input type=submit value=Upload>
        </form>
        '''
    else:
        print ('Train the model first')
        return ('No model here to use')

@app.route('/uploads/<name>')
def download_file(name):
    return send_from_directory(app.config["UPLOAD_FOLDER"], name)

if __name__ == '__main__':
    try:
        port = int(sys.argv[1]) # This is for a command-line input
    except:
        port = 12345 # If you don't provide any port the port will be set to 12345
    lr = joblib.load("model.pkl") # Load "model.pkl"
    print ('Model loaded')
    model_columns = joblib.load("model_columns.pkl") # Load "model_columns.pkl"
    print ('Model columns loaded')
    app.run(port=port, debug=True)


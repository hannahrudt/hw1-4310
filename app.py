from flask import *
from whitenoise import WhiteNoise

app = Flask(__name__)
app.wsgi_app = WhiteNoise(app.wsgi_app, 
                          root='hw1/', # use the folder name 
                          prefix='/', 
                          index_file='index.html', 
                          autorefresh=True)
    
if __name__ == "__main__":
    app.run(threaded=True, port=5000)

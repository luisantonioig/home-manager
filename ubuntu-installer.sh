# Install google-chrome
# Descargar la clave de firma
wget -q -O - https://dl.google.com/linux/linux_signing_key.pub | sudo gpg --dearmor -o /usr/share/keyrings/google-chrome-keyring.gpg

# Agregar el repositorio
echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-chrome-keyring.gpg] http://dl.google.com/linux/chrome/deb/ stable main" | sudo tee /etc/apt/sources.list.d/google-chrome.list

# Actualizar la lista de paquetes
sudo apt update

# Instalar Google Chrome
sudo apt install google-chrome-stable

# Install slack
# Descargar la clave de firma
wget -q -O - https://packagecloud.io/slacktechnologies/slack/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/slack-keyring.gpg

# Agregar el repositorio
echo "deb [arch=amd64 signed-by=/usr/share/keyrings/slack-keyring.gpg] https://packagecloud.io/slacktechnologies/slack/debian/ jessie main" | sudo tee /etc/apt/sources.list.d/slack.list

# Actualizar la lista de paquetes
sudo apt update

# Instalar Slack
sudo apt install slack-desktop


# Install emacs
sudo apt install emacs

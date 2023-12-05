$(document).ready(function () {
    $(".btn-submit").click(function () {
        console.log("Button Click")
    });


    $(".btn-download-hydrodynamic").click(function () {
        fetch('api/v1/items/files/zip')
            .then(response => response.blob())
            .then(blob => {
                const url = window.URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = 'result.zip';
                a.click();
            })
    });
});

// Fetch Logs
fetch('api/v1/items/logs')
    .then(response => response.text())
    .then(data => {
        const fileContentElement = document.getElementById('file-content');
        if (fileContentElement) {
            fileContentElement.textContent = data;
        }
    })
    .catch(error => console.error('Error fetching file:', error));


function displayFileName() {
    const fileInput = document.getElementById('fileInput');
    const fileNameDisplay = document.getElementById('fileName');
    const fileName = fileInput.files[0].name;
    fileNameDisplay.textContent = `   ${fileName}`;
}
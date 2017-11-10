function errorAlert() {
    return $('<div class="alert alert-danger alert-dismissible fade in" role="alert">\n    An error has occurred.\n    <button type="button" class="close" data-dismiss="alert" aria-label="Close">\n        <span aria-hidden="true">&times;</span>\n    </button>\n</div>');
}

function successAlert() {
    return $('<div class="alert alert-success alert-dismissible fade in" role="alert">\n    Successfully saved.\n    <button type="button" class="close" data-dismiss="alert" aria-label="Close">\n        <span aria-hidden="true">&times;</span>\n    </button>\n</div>');
}

function showAlert(alert, container) {
    alert.appendTo(container).alert().addClass("show");
    setTimeout(function () {
        alert.alert('close');
    }, 3000)
}

(function () {
    var readKey = $('#readKey'), readValue = $("#readValue"), readForm = $('#readForm');
    var readAlert = readForm.find('.alert-container');

    readForm.submit(function (e) {
        var key = readKey.val();
        $.get('/get/' + key).then(function (v) {
            readValue.val(v);
        }, function (v) {
            console.error(v);
            showAlert(errorAlert(), readAlert);
        });
        e.preventDefault()
    });


    var writeForm = $('#writeForm'), writeKey = $('#writeKey'), writeValue = $("#writeValue");
    var writeAlert = writeForm.find('.alert-container');

    writeForm.submit(function (e) {
        var key = writeKey.val(),
            newVal = writeValue.val();
        $.get('/put/' + key + "/" + newVal).then(function () {
            showAlert(successAlert(), writeAlert);
        }, function (v) {
            console.error(v);
            showAlert(errorAlert(), writeAlert);
        });
        e.preventDefault()
    });

})();
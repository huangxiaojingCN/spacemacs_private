;;; -*- lexical-binding: t -*-
(require 'request)
(require 'counsel)
(require 'helm)
(defvar carlos/gitlab-projectid nil)
(setq carlos/gitlab-cached-selected-milestone "")
(setq carlos/gitlab-add-issue-last-choose-project "")
(setq carlos/gitlab-add-issue-last-choose-duedate nil)

(setq carlos/last-choose-gitlab-worker "")
(defun carlos/helm-choose-worker ()
  (interactive)
  (helm :sources (helm-build-sync-source "*helm choose gitlab worker*"
                   :candidates  carlos/gitlab-work-labels
                   :fuzzy-match t
                   :action (lambda (condicate)
                             (message "helm choose worker is:%s save gitlab worker:%s " condicate (nth 0 condicate))
                             (setq carlos/last-choose-gitlab-worker (nth 0 condicate))
                             condicate
                             ))
        :buffer "*helm choose gitlab worker*"
        :preselect carlos/last-choose-gitlab-worker
        ))

(defun carlos/helm-debug-choose-menu ()
  (interactive)
  (message "choose :%s" (helm :sources '(font-helm-source)
                              :buffer "*helm my command"
                              )))

(defun carlos/helm-debug-fuzzy-choose ()
  (interactive)
  (message "fuzzy choose:%s" (helm :sources (helm-build-sync-source "test"
                                              :candidates  gitlab-project-sources
                                              :fuzzy-match t)
                                   :buffer "*helm test*")))

(defun carlos/helm-choose-which-gitlabproject ()
  (interactive)

  (if (carlos/org-get-project-name-local-value)
      (setq carlos/gitlab-add-issue-last-choose-project (carlos/org-get-project-name-local-value))
      )
  (helm :sources (helm-build-sync-source "*helm gitlab choose projects*"
                   :candidates  gitlab-project-sources
                   :fuzzy-match t
                   :action (lambda (condicate)

                             (setq carlos/gitlab-add-issue-last-choose-project (car condicate))
                             (cdr condicate)
                             )
                   )
        :input carlos/gitlab-add-issue-last-choose-project
        :buffer "*helm gitlab choose projects*" ))

(defun carlos/gitlab-choose-gitlabproject ()
  (interactive)
  (if (carlos/org-get-project-name-local-value)
      (setq carlos/gitlab-add-issue-last-choose-project (carlos/org-get-project-name-local-value)))
  (helm :sources (helm-build-sync-source "*helm gitlab choose projects*"
                   :candidates  gitlab-project-sources
                   :fuzzy-match t
                   :action (lambda (condicate)

                             (setq carlos/gitlab-add-issue-last-choose-project (car condicate))
                             condicate
                             )
                   )
        :input carlos/gitlab-add-issue-last-choose-project
        :buffer "*helm gitlab choose projects*" ))

(setq carlos/gitlab-query-issue-last-choose-project "")

(defun carlos/invalid-gitlab-choose-milestone ()
  "docstring"
  (setq carlos/gitlab-query-last-milestone '())
  ;; (setq carlos/gitlab-query-last-milestone-timestamp '())
  (setq cache_milestone  nil)
  ;; (setq cache_milestone_timestamp  '())
  )

(defun carlos/invalid-gitlab-cached ()
  "docstring"
  (interactive "")
  (setq carlos-cached-todo-history nil )
  (setq carlos/gitlab-get-assign-without-qa-cached nil)
  (carlos/invalid-gitlab-choose-milestone)
  ;;(carlos/invalid_gitlab_choose_cache)

  (setq carlos/gitlab-query-last-issues '())
  ;; (setq carlos/gitlab-query-last-issues-timestamp '())

  (setq carlos/gitlab-browse-todos-offset 0)

  (setq carlos/gitlab-browse-todos-history nil)

  (setq carlos/gitlab-browse-todos-pagesize 10)

  (setq carlos/browser-url-sleep 0.168)
  ;; (carlos/invalid-gitlab-choose-milestone)
  (setq carlos/gitlab-query-last-milestone '())
  ;; (setq carlos/gitlab-query-last-milestone-timestamp '())
  (setq carlos/gitlab-not-assign-qa-issue-list nil)

  (setq carlos/gitlab-issues-assign-to-me-cached nil)

  (setq carlos/gitlab-add-issue-last-choose-duedate nil)

  )

(carlos/invalid-gitlab-cached)

(defun carlos/helm-choose-which-gitlabproject-to-query ()
  (interactive)
  (if (carlos/org-get-project-name-local-value)
      (setq carlos/gitlab-query-issue-last-choose-project (carlos/org-get-project-name-local-value)))
  (helm :sources (helm-build-sync-source "*helm gitlab choose projects*"
                   :candidates  gitlab-project-sources
                   :fuzzy-match t
                   :action (lambda (condicate)

                             (setq carlos/gitlab-query-issue-last-choose-project (car condicate))
                             (cdr condicate)
                             )
                   )
        :input carlos/gitlab-query-issue-last-choose-project
        :buffer "*helm gitlab choose projects*" ))

(defun carlos/query-issue-str ()
  (setq query-str (carlos/get-mark-term))
  (if (or (eq query-str nil) (string= query-str ""))
      (progn
        (setq query-str (replace-regexp-in-string "*" ""
                                                  (replace-regexp-in-string "LABELED" "" (thing-at-point 'line)))))))

(defun carlos/gitlab-handle-queried-cached (carlos/gitlab-projectid carlos/gitlab-query-last-issues callback)
  "docstring"
  (progn
    (message "carlos call handled queried cached projectid is:%s" carlos/gitlab-projectid)
    ;; (message "carlos query last issues is:%s" carlos/gitlab-query-last-issues)
    (setq carlos/gitlab-call-http-request nil)
    (setq carlos/gitlab-choosed-issue-by-query
          (helm :sources (helm-build-sync-source "helm-gitlab-issue-result"
                           :candidates  (cdr (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues))
                           :action (helm-make-actions "Close Selected Issue"   (lambda (candidate)
                                                                                 (cond ((equal "closed" (nth 1 candidate))
                                                                                        (org-todo "DONE")
                                                                                        ))
                                                                                 t
                                                                                 ;; (funcall callback)
                                                                                 )
                                                      "Open Select Issue"  (lambda (candidate)
                                                                             (browse-url (nth 0 candidate))
                                                                             )
                                                      )
                           :fuzzy-match t)
                :input (carlos/gitlab-filter-special-char (org-get-heading t t))
                ;; :input  (carlos/split_string  (trim-string carlos-get-mark-term) 64)
                :buffer "*helm gitlab projects*" ))
    ;; (message "debug choosed issue by query is:%s" carlos/gitlab-choosed-issue-by-query)
    ;; (message "carlos dump choose issue")
    ;; (message "callback is:%s" callback)
    (funcall callback carlos/gitlab-choosed-issue-by-query)
    ;; (if (equal  nil carlos/gitlab-choosed-issue-by-query)
    ;;     )
    ))

(defun carlos/gitlab-handle-queried-issue-finish (carlos/gitlab-projectid data callback)
  "docstring"
  (progn
    (message "Handle Query result")
    (setq carlos/debug/gitlab/issue/query-result data)
    (setq search-source '())
    (setq carlos/gitlab-del-issue-worker-label "")
    (cl-loop for i across cur_page_concat
             do (progn
                  (setq converted-item "")
                  (cl-loop for j across (cdr (assq 'labels i))
                           do (progn
                                (setq converted-item (concat (decode-coding-string j 'utf-8-emacs) " " converted-item))
                                (setq carlos/gitlab-del-issue-worker-label (decode-coding-string j 'utf-8-emacs))
                                ))
                  (setq converted-item (concat converted-item (replace-regexp-in-string "\n" "" (concat (cdr (assq 'state i))  " " (decode-coding-string (cdr (assq 'title i)) 'utf-8-emacs)))))
                  (setq search-source
                        (append search-source
                                (list (cons converted-item (list  (cdr (assq 'web_url i)) (cdr (assq 'state i)) (cdr (assq 'id i)) (cdr (assq 'iid i)) (cdr (assq 'project_id i)) converted-item carlos/gitlab-del-issue-worker-label) ))))))
    (if (not (equal nil (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues)))
        (progn
          (message "project %s is queried cached update" carlos/gitlab-projectid)
          (setcdr (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues) search-source)
          )
      (progn
        (message "project %s is not queried append it" carlos/gitlab-projectid)
        (setq carlos/gitlab-query-last-issues (append  carlos/gitlab-query-last-issues (list (cons carlos/gitlab-projectid search-source))))
        )
      )
    ;; (if (not (equal nil (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues-timestamp)))
    ;;     (setcdr (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues-timestamp) (time-to-seconds))
    ;;   (setq carlos/gitlab-query-last-issues-timestamp (append carlos/gitlab-query-last-issues-timestamp (list (cons carlos/gitlab-projectid (time-to-seconds)))))
    ;;   )
    (setq carlos/debug/gitlab/issue/query-result-search-source search-source)
    (setq carlos/gitlab-choosed-issue-by-query (helm :sources (helm-build-sync-source "helm-gitlab-issue-result"
                                                                :candidates  search-source
                                                                :fuzzy-match t
                                                                :action (helm-make-actions "Close Selected Issue"   (lambda (candidate)
                                                                                                                      (cond ((equal "closed" (nth 1 candidate))
                                                                                                                             (org-todo "DONE")
                                                                                                                             ))
                                                                                                                      t
                                                                                                                      ;; (funcall callback)
                                                                                                                      ;; (browse-url candidate)
                                                                                                                      )
                                                                                           "Open Select Issue"  (lambda (candidate)
                                                                                                                  (browse-url (nth 0 candidate))
                                                                                                                  )
                                                                                           "Del Select Issue"  (lambda (candidate)
                                                                                                                 (message "try to delete %s" candidate)
                                                                                                                 (setq carlos/gitlab-del-issue-worker (nth 6 candidate))

                                                                                                                 (request
                                                                                                                  (format "%s/projects/%s/issues/%s" carlos/gitlab-default-gitlaburl  (nth 4 candidate) (nth 3 candidate))
                                                                                                                  :type "DELETE"
                                                                                                                  :parser 'json-read
                                                                                                                  :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
                                                                                                                  :success (cl-function
                                                                                                                            (lambda (&key data &allow-other-keys)
                                                                                                                              (message "Del issue success :%s" data)
                                                                                                                              (org-todo "TODO")
                                                                                                                              (carlos/gitlab_query_labeled_issue carlos/gitlab-del-issue-worker nil 1 (vector) 6 36  '(lambda (issue-list labeled)
                                                                                                                                                                                                                        (progn
                                                                                                                                                                                                                          (message "query worker arrange issue")
                                                                                                                                                                                                                          (carlos/gitlab-handle-issue-output-after-add-issue issue-list labeled)
                                                                                                                                                                                                                          ;; (carlos/gitlab-handle-issue-output issue-list labeled)
                                                                                                                                                                                                                          )))
                                                                                                                              ))
                                                                                                                  :error (cl-function
                                                                                                                          (lambda (&key data &allow-other-keys)
                                                                                                                            (message "del issue fail %s" data)
                                                                                                                            (carlos/gitlab_query_labeled_issue carlos/gitlab-del-issue-worker nil 1 (vector) 6 36  '(lambda (issue-list labeled)
                                                                                                                                                                                                                      (progn
                                                                                                                                                                                                                        (message "query worker arrange issue")
                                                                                                                                                                                                                        (carlos/gitlab-handle-issue-output-after-add-issue issue-list labeled)
                                                                                                                                                                                                                        ;; (carlos/gitlab-handle-issue-output issue-list labeled)
                                                                                                                                                                                                                        )))
                                                                                                                            ))))))
                                                     ;; :input (carlos/split_string (trim-string carlos-get-mark-term) 64)
                                                     :input (org-get-heading t t)
                                                     :buffer "*helm gitlab projects*" ))
    (message "carlos debug choose issue by query :%s" carlos/gitlab-choosed-issue-by-query)
    ;; (if (equal nil carlos/gitlab-choosed-issue-by-query)
    ;;     (funcall callback))
    (funcall callback carlos/gitlab-choosed-issue-by-query)
    )
  )

(defun carlos/query-gitlab-issues-fn (page_index page_concat callback)
  (setq carlos/gitlab-call-http-request t)
  (if (= page_index -1)
      (progn
        ;; (setq carlos-get-mark-term  "");;(or (carlos/query-issue-str) ""))
        (setq carlos-query-str nil)
        (setq carlos/gitlab-projectid (carlos/helm-choose-which-gitlabproject-to-query))
        (setq carlos/gitlab-current-time (time-to-seconds))
        (setq page_index 0)
        (message "query gitlab issuess select projectid:%s" carlos/gitlab-projectid)
        (if (not (equal nil (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues)))
            (carlos/gitlab-handle-queried-cached carlos/gitlab-projectid carlos/gitlab-query-last-issues callback)
            )
        ;; (if (not (equal (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues-timestamp) nil))
        ;;     (progn
        ;;       (if (< (- carlos/gitlab-current-time (cdr  (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues-timestamp))) (* 60 60 ))
        ;;           (carlos/gitlab-handle-queried-cached carlos/gitlab-projectid carlos/gitlab-query-last-issues callback)
        ;;           )))
        ))

  (setq cur_page_index (+ page_index 1))
  (setq cur_page_concat page_concat)
  (if (equal carlos/gitlab-call-http-request t)
      (progn
        (message "Querying Data From Internet... Cur_index:%s Readed:%s" cur_page_index (length cur_page_concat))
        (request
         (concat carlos/gitlab-default-gitlaburl "/projects/" carlos/gitlab-projectid "/issues")
         :type "GET"
         :parser 'json-read
         :data (list (cons "page" cur_page_index) (cons "per_page"  90) (cons "order_by" "updated_at") (cons "sort"  "desc"))
         :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
         :success (cl-function
                   (lambda (&key data &allow-other-keys)
                     (message "Get Query Data length is:%s" (length data))
                     ;; (setq carlos-item (aref data 0))
                     ;; (setq carlos-issue-content (cdr (assq 'title carlos-item)))
                     (setq cur_page_concat (vconcat cur_page_concat data))
                     (cond ((>= (length data) 90)
                            (message "Keep fetching")

                            (carlos/query-gitlab-issues-fn cur_page_index cur_page_concat callback)
                            )
                           (t
                            (carlos/gitlab-handle-queried-issue-finish carlos/gitlab-projectid data callback)))))
         :error (cl-function
                 (lambda (&key data &allow-other-keys)
                   (message "Query Issue Fail : %S" data)))))))

(defun carlos/Query-gitlab-issues ()
  (interactive )
  (carlos/query-gitlab-issues-fn -1 (vector) (lambda (matched) (interactive ""))))

(defun carlos/gitlab-add-issue-with-milestone ()
  (interactive)
  (carlos/query-gitlab-issues-fn -1 (vector) (lambda (matched) (interactive "")
                                               (if (equal nil matched)
                                                   (carlos/gitlab-add-dev-issue-with-project-milestone)
                                                 (kill-whole-line)))))

(defun carlos/gitlab-add-issue (milestone project_id &optional target_due_date target_due_date_index)
  (setq choose_due_date_str_format "")
  (setq worker_label nil)
  (setq issue_param '())
  (setq carlos/gitlab-add-issue-target-due-date (or target_due_date "Sat"))
  (setq target_due_date_index (or target_due_date_index 0))
  (setq carlos/debug-gitlab-add-issue-choose-milestone milestone)
  (if (and (not (equal milestone nil)) (not (equal (car milestone) nil)))
      (setq issue_param (append issue_param (list (cons "milestone_id" (car  milestone)))))
    )
  (unwind-protect
      (progn
        ;; (message "debug add issue choose milestone:%s" (or milestone (cons 0 (format-time-string "%Y-%m-%d"))))
        (setq choose_due_date_str_format (cdr (or milestone (cons 0 (format-time-string "%Y-%m-%d")))))
        ;; (message "debug get current heding include test:%s heading is:%s" (string-match "测试 .*" (or (org-get-heading t t) "")) (org-get-heading t t))
        (setq choose_due_date_next_sat (carlos/get-last-week-work-day (format-time-string "%Y-%m-%d" (time-add (carlos/parse-time (format-time-string "%Y-%m-%d")) (* 10 (* 60 60 24))))
                                                             (format-time-string "%Y-%m-%d" (time-add (carlos/parse-time (format-time-string "%Y-%m-%d")) (* 2 (* 60 60 24))))
                                                             "6" 1))
        ;; (if (string-match "测试 .*" (or (org-get-heading t t) ""))
        ;;     (progn
        ;;       (setq choose_due_date_str_format (carlos/gitlab-get-next-saturday (cdr milestone)))))
        (if (time-less-p (carlos/parse-time choose_due_date_str_format) (carlos/parse-time choose_due_date_next_sat))
            (setq choose_due_date_str_format choose_due_date_next_sat)
          )
        ;; (message "Debug last choose duedate is:%s" carlos/gitlab-add-issue-last-choose-duedate)
        (if carlos/gitlab-add-issue-last-choose-duedate
            (setq choose_due_date (org-read-date nil nil nil nil nil carlos/gitlab-add-issue-last-choose-duedate))
            (setq choose_due_date (org-read-date nil nil nil nil nil choose_due_date_str_format))
            )
        ;; (message "after choose_due_date is:%s" choose_due_date)
        (setq carlos/gitlab-add-issue-last-choose-duedate (format "%s" choose_due_date))
        ;; (if (not carlos/gitlab-add-issue-last-choose-duedate)
            ;; (setq carlos/gitlab-add-iss))
        )
    (message "debug get milestone choose_due_date is:%s milestone is:%s" choose_due_date milestone)
    (if (not (string= choose_due_date ""))
        (progn
          (message "choose due date is:%s" choose_due_date)
          (setq issue_param (append issue_param (list (cons "due_date" choose_due_date))))
          )
      )
    (setq choose_worker (carlos/helm-choose-worker))
    (setq worker_label (nth 0 choose_worker))
    (setq carlos/gitlab-add-issu-worker-account (nth 1 choose_worker))
    (if (not (string= worker_label nil))
        (progn
          (setq issue_param (append issue_param (list (cons "labels" worker_label))))))
    (if (not (equal -1 project_id))
        (setq carlos/gitlab-projectid project_id))
    (if (equal nil project_id)
        (setq carlos/gitlab-projectid (carlos/helm-choose-which-gitlabproject)))
    (if (and (not (equal nil carlos/gitlab-projectid)) (not (string= worker_label nil)))
        (carlos/helm-add-issues-with-param issue_param carlos/gitlab-projectid))))

(defvar carlos/gitlab-add-issue-query-enable nil "Query work planed issues before add?")

(defun carlos/helm-add-issues-with-param (issue_param carlos/gitlab-projectid)
  (progn
    (message "add issue param  %s " issue_param)
    (setq mark_string (replace-regexp-in-string "[+]" " " (org-get-heading t t)))
    (setq mark_string (concat mark_string " #emacs"))
    (cond ((not (string= mark_string ""))
           (if (eq nil carlos/gitlab-projectid)
               (progn
                 (message "you should  choose a nil string to create issue")
                 )
             (progn
               (setq labeledworker (cdr (assoc "labels" issue_param)))
               (setq issue_param (append issue_param (list (cons "id" carlos/gitlab-projectid) (cons "title"  mark_string) (cons "description" (format "/assign @%s \n @%s #due:%s" carlos/gitlab-add-issu-worker-account carlos/gitlab-add-issu-worker-account (cdr (assoc "due_date" issue_param)) )))))
               (message "Sending Issue to GitLab... issue_param:%s hook is:%s" issue_param carlos/gitlab-add-issue-query-enable)
               (setq carlos/gitlab-debug-parse-label-due-date issue_param)
               ;; (carlos/gitlab-get-user-and-plan-time-from-param carlos/gitlab-debug-parse-label-due-date)
               (if carlos/gitlab-add-issue-query-enable
                   (progn
                     (let ((parse-user-plan-date (carlos/gitlab-get-user-and-plan-time-from-param issue_param)))
                       (setq carlos/gitlab-debug-parse-user-plan-date parse-user-plan-date)
                       (let ((user (nth 0 parse-user-plan-date))
                             (due_date (carlos/funs-convert-string-to-year-week (nth 1 parse-user-plan-date))))
                         (carlos/gitlab-fetch-user-plan-by-carlos due_date
                                                                  user (list) 0 90 (lambda (data)
                                                                                     (let ((data (carlos/gitlab-filter-without-preclose-issue data)))

                                                                                       (if (< carlos/gitlab-weekly-max-plan-issue-number (length data))
                                                                                           (progn
                                                                                             (message "task number is overlaod try to ask")
                                                                                             (if (y-or-n-p (format "The user %s week %s plan task number is:%s do you want to force added?" user due_date (length data) ))
                                                                                                 (progn
                                                                                                   (carlos/gitlab-sent-issue-and-auto-label carlos/gitlab-projectid issue_param data user due_date)
                                                                                                   )))
                                                                                         (progn

                                                                                           (carlos/gitlab-sent-issue-and-auto-label carlos/gitlab-projectid issue_param data user due_date)
                                                                                           )))))
                         ))
                     )
                 (progn
                   (progn
                     (let ((parse-user-plan-date (carlos/gitlab-get-user-and-plan-time-from-param issue_param)))
                       (setq carlos/gitlab-debug-parse-user-plan-date parse-user-plan-date)
                       (let ((user (nth 0 parse-user-plan-date))
                             (due_date (carlos/funs-convert-string-to-year-week (nth 1 parse-user-plan-date))))
                         (carlos/gitlab-sent-issue-and-auto-label carlos/gitlab-projectid issue_param (list ) user due_date)
                         ))
                     )
                   )
                 )
               )))
          (t
           (message-box "you should choose something to sent to gitlab")))))

(defun carlos/gitlab-filter-without-preclose-issue (history)
  "docstring"
  (let ((valid-history (delq nil (mapcar (lambda (issue)
                                           (let ((close-week (cdr (assoc 'last_closed_at_week_int  issue)))
                                                 (due-week (cdr (assoc 'due_week_year_int  issue))))
                                             (if (and (not (equal close-week 0)) (< close-week due-week))
                                                 nil
                                               issue )))
                                         history))))
    valid-history
    ))

(defun carlos/gitlab-local-force-fetch-event ()
  "用于添加工单之后触发本地服务立刻请求 event 数据"
  ;; (interactive "")
  (request
   (url-encode-url  (format "http://%s:10089/triggerFetchEvent?secret_key=%s" carlos/gitlab-local-server-url carlos/gitlab-feed-bot-token ))
   :type "GET"
   :parser 'json-read
   :success (cl-function
             (lambda (&key data symbol-status error-thrown  response &all-other-keys )
               ;; (message "force gitlab fedd bot fetch event status:%s" symbol-status)
               ))
   :error (cl-function
           (lambda (&key data symbol-status error-thrown response &all-other-keys)
             (message "debug trigger fetch event err %s status:%s error-thrown:%s response:%s" data symbol-status error-thrown response)))))

(setq carlos/gitlab-weekly-max-plan-issue-number 11)

(defvar carlos/gitlab-feed-bot-token nil )

(defun carlos/gitlab-fetch-user-plan-by-carlos (curweek user history pageindex pagesize callback)
  "docstring"
  (request
   (url-encode-url (format "http://%s:10089/GetUserPlan?label=%s&week=%s&pagesize=%d&pageoffset=%d&secret_key=%s" carlos/gitlab-local-server-url (decode-coding-string user 'utf-8)  curweek pagesize pageindex (or carlos/gitlab-feed-bot-token "") ))
   :type "GET"
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)

               (let ((history (append data history)))
                 (if (<= pagesize (length data))
                     (carlos/gitlab-fetch-user-plan-by-carlos curweek user history (+ 1 pageindex) pagesize callback)
                   (progn

                     (funcall callback history)
                     )))))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "Query worker arrange issue fail error:%s" data)))))

(defun carlos/gitlab-sent-issue-and-auto-label (carlos/gitlab-projectid issue_param planed-issues user due_date)
  (request
   (concat carlos/gitlab-default-gitlaburl "/projects/" carlos/gitlab-projectid "/issues")
   :type "POST"
   :data issue_param
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (if carlos/gitlab-add-issue-query-enable
                   (progn
                     (carlos/gitlab-local-force-fetch-event)
                     (message "Fetch user %s Planed  at W:%s without preclose TaskNum:%s" user due_date (length planed-issues))
                     (sleep-for 3.0)
                     (let ((i 0)
                           (count-length 6))
                       (mapc (lambda (data)
                               (setq i (+ 1 i))
                               (sleep-for 1.0)
                               (message "Wait in:%s seconds" (- count-length i))
                               )
                             (make-list count-length 'sleep-for))
                       )
                     )
                   )
               ;; (sleep-for 8.0) ;; delay for show the worker planed issue number
               ;; (sleep-for 2.0)
               (setq carlos/debug/gitlab/issue-add-result-data data)
               (carlos/gitlab-update-query-issue-cache carlos/gitlab-projectid data)
               (message "Debug Issue Added Success Id is:%s" (cdr (assoc 'iid data)))
               (cond ((not (equal nil (cdr (assoc 'labels data))))
                      (progn
                        (message "labeled heading")
                        (message "after labeled heading")
                        )
                      ))
               (message "labels of added is:%s" (assoc 'labels data))
               (when (not  (equal (assoc 'labels data) nil))
                 (org-todo "LABELED")
                 (message "Labeled current heading :%s" (org-get-heading t t))
                 (carlos/gitlab-get-QA-Bug-link (org-get-heading t t))
                 ;; (org-cycle 'FOLDED)
                 ;; (outline-next-heading)
                 
                 )
               ))

   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "Issue sent fail : %s" data)))))

(defun carlos/gitlab-handle-issue-output (issue-list labeled)
  (setq carlos/gitlab-handle-issue-list issue-list)
  (setq carlos/gitlab-handle-issue-labeled labeled)
  (message "try to get work opend issue api is:%s" (concat carlos/gitlab-default-gitlaburl (format "/issues?state=opened&labels=%s" labeled)))
  (request
   (concat carlos/gitlab-default-gitlaburl (format "/issues?state=opened&labels=%s" labeled))
   :type "GET"
   :data (list (cons "page" 1) (cons "per_page"  100))
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "Get Opened Issue")
               (setq carlos/gitlab-handle-issue-data-length (length data))
               (setq issue-list carlos/gitlab-handle-issue-list)
               (let ((cur_week (org-days-to-iso-week (- (time-to-day-in-year (org-current-time)) 1)))
                     (output (format "名称:%s 剩余工单:%s->" carlos/gitlab-handle-issue-labeled carlos/gitlab-handle-issue-data-length))
                     (schdule-todo-count 0)
                     )
                 (mapc (lambda (data) (interactive "")
                         (cond ( (assoc (format "week-%d" data) issue-list)
                                 (let ((week-data (cdr (assoc (format "week-%d" data) issue-list))))

                                   (cond ((equal data cur_week)
                                          (setq output (concat output  (propertize
                                                                        (format " week-%s 延迟关闭:%s 正常关闭:%s 提前关闭:%s 计划:%s 本周工作情况 %s/%s |" data (elt week-data 0) (elt week-data 1) (elt week-data 2) (elt week-data 3) (elt week-data 5) (- (elt week-data 3) (elt week-data 2)))
                                                                        'face '(:foreground "DarkGoldenrod2" :weight 'bold)
                                                                        'help-echo "buffer is read-only!!!")))
                                          )
                                         (t
                                          (setq output (concat output (format " week-%s 延迟关闭:%s 正常关闭:%s 提前关闭:%s 计划:%s 本周工作情况 %s/%s |" data (elt week-data 0) (elt week-data 1) (elt week-data 2) (elt week-data 3) (elt week-data 5) (- (elt week-data 3) (elt week-data 2)))
                                                               ) )
                                          )
                                         )
                                   (cond ((>= data cur_week)
                                          (setq schdule-todo-count (+ schdule-todo-count (elt week-data 5)))
                                          ))))))
                       (number-sequence (- cur_week 4) (+ cur_week 4)))
                 (switch-to-buffer "* Worker Report *")
                 (goto-char (point-max))
                 (let ((last-week-data (assoc (format "week-%d" (- cur_week 1)) issue-list))
                       (cur-week-data (assoc (format "week-%d" cur_week) issue-list))
                       )
                   (cond ((equal nil last-week-data)
                          (setq last-week-data (cons (format "week-%s" (- cur_week 1)) (vector 0 0 0 0 0 0 )))
                          ))
                   (cond ((equal nil cur-week-data)
                          ;; (setq output (append output (list (cons (format "week-%s" issue_due_date) (vector 0 0 0 0 0 0)))))
                          (progn
                            (setq cur-week-data (cons "week-31"  (vector 0 0 0 0 0 0))) ;;延迟关闭 计划内关闭 提前关闭 本周计划工单 本周扣去提前关闭计划工单; 本周剩余工单
                            )))
                   (let ((delay-before-cur-week (- carlos/gitlab-handle-issue-data-length (or schdule-todo-count 0) ))
                         (pre-close-percent (/   (float (elt (cdr cur-week-data) 2)) 13))
                         )
                     (cond ((> 0 delay-before-cur-week)
                            (progn
                              (setq pre-close-percent (/ (- (float (elt (cdr cur-week-data) 2)) delay-before-cur-week) 13))
                              )
                            ))
                     (insert  (format "%s \n计划内未完成:%s 延迟工单:%s 提前完成比例:%1.2f\n\n" output schdule-todo-count delay-before-cur-week pre-close-percent))
                     )))))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "get issue opened data fail %s" data)
             ))))

(defun carlos/gitlab-handle-issue-output-after-add-issue (issue-list labeled)
  (setq carlos/gitlab-handle-issue-list issue-list)
  (setq carlos/gitlab-handle-issue-labeled labeled)
  (message "try to get work opend issue api is:%s" (concat carlos/gitlab-default-gitlaburl (format "/issues?state=opened&labels=%s" labeled)))
  (request
   (concat carlos/gitlab-default-gitlaburl (format "/issues?state=opened&labels=%s" labeled))
   :type "GET"
   :data (list (cons "page" 1) (cons "per_page"  100))
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "get issue data")
               (setq carlos/gitlab-handle-issue-data-length (length data))
               (setq issue-list carlos/gitlab-handle-issue-list)
               (let ((cur_week (org-days-to-iso-week (- (time-to-day-in-year (org-current-time)) 1)))
                     (output (format "名称:%s 剩余工单:%s->" carlos/gitlab-handle-issue-labeled carlos/gitlab-handle-issue-data-length))
                     (schdule-todo-count 0)
                     (normal-schdule-tood-count 0)
                     )
                 (mapc (lambda (data) (interactive "")
                         (cond ( (assoc (format "week-%d" data) issue-list)
                                 (let ((week-data (cdr (assoc (format "week-%d" data) issue-list))))

                                   (cond ((equal data cur_week)
                                          (setq output (concat output  (propertize
                                                                        (format " week-%s DC:%s NC:%s PC:%s Plan:%s Status::%s/%s |" data (elt week-data 0) (elt week-data 1) (elt week-data 2) (elt week-data 3) (elt week-data 5) (- (elt week-data 3) (elt week-data 2))  )
                                                                        'face '(:foreground "DarkGoldenrod2" :weight 'bold)
                                                                        'help-echo "buffer is read-only!!!")))
                                          )
                                         (t
                                          (setq output (concat output (format " week-%s DC:%s NC:%s PC:%s Plan:%s Status::%s/%s |" data (elt week-data 0) (elt week-data 1) (elt week-data 2) (elt week-data 3) (elt week-data 5) (- (elt week-data 3) (elt week-data 2))  )
                                                               ) )
                                          )
                                         )
                                   (cond ((>= data cur_week)
                                          (progn
                                            (setq schdule-todo-count (+ schdule-todo-count (elt week-data 5)))
                                            (message "add schdule todo count is:%s" schdule-todo-count)
                                            )
                                          )))))
                         ;; (setq normal-schdule-tood-count  (+  normal-schdule-tood-count (elt week-data 5)))
                         )
                       (number-sequence (- cur_week 2) (+ cur_week 16)))
                 (message "after handle note schduel-todo-count is:%s" schdule-todo-count)
                 (let ((last-week-data (assoc (format "week-%d" (- cur_week 1)) issue-list))
                       (cur-week-data (assoc (format "week-%d" cur_week) issue-list))
                       )
                   (cond ((equal last-week-data nil)
                          (setq last-week-data (cons (format "week-%d" (- cur_week 1)) (vector 0 0 0 0 0 0)))
                          ))
                   (message "try to handle delay before current week cur-week-date is:%s" cur-week-data)
                   (message "debug last-week-data is:%s" last-week-data)
                   (let ((delay-before-cur-week (- carlos/gitlab-handle-issue-data-length (or schdule-todo-count 0) ))
                         (pre-close-percent (/   (float (elt (cdr cur-week-data) 2)) (- (elt (cdr last-week-data) 3) (elt (cdr last-week-data) 2))))
                         )
                     (message "handle output of message")
                     (cond ((> 0 delay-before-cur-week)
                            (progn
                              (setq pre-close-percent (/ (- (float (elt (cdr cur-week-data) 2)) delay-before-cur-week) (- (elt (cdr last-week-data) 3) (elt (cdr last-week-data) 2))))
                              )
                            ))
                     (message "%s >>> 计划内未完成:%s 延迟工单:%s" output schdule-todo-count delay-before-cur-week)
                     ))
                 )))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "get issue data fail %s" data)
             ))))

(defun carlos/gitlab_handle_issue_notes (issues_list handled_issues labeled finish-callback)
  (message "carlos gitlab handle issue left:%s handle note length is:%s " (length issues_list) (length handled_issues) )
  (setq carlos/gitlab_handle_issues_notes_issues_list (cdr issues_list))
  (setq carlos/gitlab_handle_issues_notes_handled_issues handled_issues)
  (setq carlos/gitlab_handle_issues_notes_handled_cur_issue (car issues_list))
  (setq carlos/gitlab-query-opened-issue-labeled-worker labeled)
  (setq carlos/gitlab-query-opened-issue-finish-callback finish-callback)

  ;; (let ((cur-issue carlos/gitlab_handle_issues_notes_handled_cur_issue))

  ;;   )
  (request
   (concat carlos/gitlab-default-gitlaburl (format "/projects/%s/issues/%s/notes" (cdr (assoc 'project_id carlos/gitlab_handle_issues_notes_handled_cur_issue)) (cdr (assoc 'iid carlos/gitlab_handle_issues_notes_handled_cur_issue))))
   :type "GET"
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (cond ((string-equal "closed" (cdr (assoc 'state carlos/gitlab_handle_issues_notes_handled_cur_issue)))
                      (progn
                        (let ((cur_issue carlos/gitlab_handle_issues_notes_handled_cur_issue))
                          (let ((issue_state (cdr (assoc 'state cur_issue)))
                                (issue_due_date (or  (org-days-to-iso-week (- (time-to-day-in-year (carlos/parse-time (or  (cdr (assoc 'due_date cur_issue)) "1970-01-01"))) 1) ) 0))
                                (issue_closed_at -1)
                                (detect-preclose nil))
                            (mapc (lambda (data)
                                    (cond ((string-match "closed.*" (cdr (assoc 'body data)))
                                           (progn
                                             (let ((create_date (carlos/parse-time-zone (cdr (assoc 'created_at data)))))
                                               (setq closed_body_created (or (org-days-to-iso-week (-  (time-to-day-in-year create_date) 1) ) -1))
                                               ;; (if (string-equal "2017-08-05" (cdr (assoc 'due_date carlos/gitlab_handle_issues_notes_handled_cur_issue )))

                                               ;;     )
                                               (cond ((< issue_closed_at closed_body_created)
                                                      (setq issue_closed_at closed_body_created)
                                                      )))))
                                          (
                                           (string-match "#pre_close" (cdr (assoc 'body data)))
                                           (progn

                                             (let ((author (cdr (assoc 'username  (assoc 'author data)))))
                                               (setq detect-preclose t)
                                               )

                                             ))))
                                  (append data nil))
                            (cond ((and  (equal t detect-preclose) )
                                   (setq issue_closed_at (- issue_closed_at 1))
                                   )
                                  )
                            (setq carlos/gitlab_handle_issues_notes_handled_issues (append carlos/gitlab_handle_issues_notes_handled_issues (list (vector issue_state issue_due_date issue_closed_at))))))))
                     (t
                      (progn
                        (let ((cur_issue carlos/gitlab_handle_issues_notes_handled_cur_issue)
                              )
                          (let ((issue_state (cdr (assoc 'state cur_issue)))
                                (issue_due_date (or  (org-days-to-iso-week (- (time-to-day-in-year (carlos/parse-time (or  (cdr (assoc 'due_date cur_issue)) "1970-01-01"))) 1)) 0))
                                (issue_closed_at -1)
                                )
                            (setq carlos/gitlab_handle_issues_notes_handled_issues (append carlos/gitlab_handle_issues_notes_handled_issues (list (vector issue_state issue_due_date issue_closed_at)))))))))
               (cond ((< 0 (length carlos/gitlab_handle_issues_notes_issues_list))
                      (progn
                        (carlos/gitlab_handle_issue_notes carlos/gitlab_handle_issues_notes_issues_list carlos/gitlab_handle_issues_notes_handled_issues carlos/gitlab-query-opened-issue-labeled-worker carlos/gitlab-query-opened-issue-finish-callback)))
                     (t
                      (progn
                        (let ((output (list (cons "carlos"  0) )))

                          (mapc (lambda (issue) (interactive "")
                                  (let ((issue_state (elt issue 0))
                                        (issue_due_date (elt issue 1))
                                        (issue_closed (elt issue 2))
                                        )
                                    (let ((output_cur_week_due (cdr (assoc (format "week-%d" issue_due_date) output)))
                                          (output_close_week_due (cdr (assoc (format "week-%d" issue_closed) output)))
                                          )

                                      (cond ((equal nil output_cur_week_due)
                                             (setq output (append output (list (cons (format "week-%s" issue_due_date) (vector 0 0 0 0 0 0)))))
                                             (setq output_cur_week_due (vector 0 0 0 0 0 0)) ;;延迟关闭 计划内关闭 提前关闭 本周计划工单 本周扣去提前关闭计划工单; 本周剩余工单
                                             ))
                                      ;; (if (not (equal issue_due_date issue_closed))
                                      (cond ((equal nil output_close_week_due)
                                             (setq output (append output (list (cons (format "week-%s" issue_closed) (vector 0 0 0 0 0 0)))))
                                             (setq output_close_week_due (vector 0 0 0 0 0 0)) ;;延迟关闭 计划内关闭 提前关闭 本周计划工单 本周扣去提前关闭计划工单; 本周剩余工单
                                             ))
                                        ;; )
                                      (cond ((string-equal issue_state "closed")
                                             (progn
                                               (if (and (equal 31 issue_due_date) t)
                                                   (message "handle closed event due_date is:%s issue_closed is:%s compare is:%s" issue_due_date issue_closed (- issue_due_date issue_closed))
                                                   )
                                               (aset output_cur_week_due 3 (+ 1 (elt output_cur_week_due 3)))
                                               (cond ((equal issue_due_date issue_closed)
                                                      (progn
                                                        (aset output_cur_week_due 1 (+ 1 (elt output_cur_week_due 1)))
                                                        (if (and (equal 31 issue_due_date) t)
                                                            (message "get close date equal to due data ")
                                                          )
                                                        )
                                                      )
                                                     ((> issue_due_date issue_closed)
                                                      (progn
                                                        (aset output_cur_week_due 2 (+ 1 (elt output_cur_week_due 2)))
                                                        )
                                                      )
                                                     ((< issue_due_date issue_closed)
                                                      (progn
                                                        (aset output_close_week_due 0 (+ 1 (elt output_close_week_due 0)))
                                                        )
                                                      )
                                                     )
                                               ))
                                            ((string-match ".*open.*" issue_state)
                                             (progn
                                               (aset output_cur_week_due 3 (+ 1 (elt output_cur_week_due 3)))
                                               (aset output_cur_week_due 5 (+ 1 (elt output_cur_week_due 5)))
                                               )))
                                      (message " before save calculate data")
                                      (setf (cdr (assoc (format "week-%d" issue_due_date) output)) output_cur_week_due)
                                      (if (not (equal issue_due_date issue_closed))
                                          (setf (cdr (assoc (format "week-%d" issue_closed) output)) output_close_week_due)
                                        )
                                      (message "after parsed dump output cur week is:%s dump output_close_week_due is:%s" (assoc (format "week-%d" issue_due_date) output) (assoc (format "week-%d" issue_closed) output))
                                      )))
                                carlos/gitlab_handle_issues_notes_handled_issues)
                          (message "Start Call handle issue output")
                          (funcall carlos/gitlab-query-opened-issue-finish-callback output carlos/gitlab-query-opened-issue-labeled-worker)
                          ))))))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "query issue note error is:%s" data)))))

(defun carlos/gitlab_query_labeled_issue (labeled addedid query_page_index history_issues issue_per_page max_issue_number finish-callback)
  (message "正在查询 %s 工单安排 刚添加工单id:%s history issue length is:%s page index is:%s " labeled addedid (length history_issues) query_page_index)
  (setq labeled_worker labeled)
  (setq carlos/last_addedid addedid)
  (setq carlos/gitlab-query-labeled-history-issues history_issues)
  (setq carlos/gitlab-query-labeled-history-issues-page-index query_page_index)
  (setq carlos/gitlab-query-opened-issue-labeled-worker labeled)
  (setq carlos/gitlab-query-issue-issue-per-page issue_per_page)
  (setq carlos/gitlab-query-issue-max-issue-number max_issue_number)
  (setq carlos/gitlab-query-issue-finish-callback finish-callback)
  (request
   (concat carlos/gitlab-default-gitlaburl "/issues?&labels=" labeled)
   :type "GET"
   :data (list (cons "order_by"  "updated_at") (cons "sort"  "desc") (cons "page" query_page_index) (cons "per_page"  carlos/gitlab-query-issue-issue-per-page) (cons "sort"  "desc") (cons "order_by"  "updated_at"))
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "Get issue data")
               (cond ( (and (<= carlos/gitlab-query-issue-issue-per-page (length data)) (> carlos/gitlab-query-issue-max-issue-number (length (vconcat carlos/gitlab-query-labeled-history-issues data))))
                       (carlos/gitlab_query_labeled_issue labeled_worker carlos/last_addedid (+ carlos/gitlab-query-labeled-history-issues-page-index 1) (vconcat carlos/gitlab-query-labeled-history-issues data) carlos/gitlab-query-issue-issue-per-page carlos/gitlab-query-issue-max-issue-number carlos/gitlab-query-issue-finish-callback)
                       )
                     (t
                      (progn
                        (carlos/gitlab_handle_issue_notes (append  (vconcat carlos/gitlab-query-labeled-history-issues data ) nil) '() carlos/gitlab-query-opened-issue-labeled-worker carlos/gitlab-query-issue-finish-callback)
                        )
                      )
                     )
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "Query issue error:%s" data)
             ))))

(defun carlos/gitlab_query_worker_arrange_issue ()
  (interactive)
  (helm :sources (helm-build-sync-source "helm-gitlab-projects"
                   :candidates  carlos/gitlab-work-labels
                   :fuzzy-match t
                   :action (lambda (condicate)
                             (message "choosed project is:%s" (nth 0 condicate))
                             (carlos/gitlab_query_labeled_issue (nth 0 condicate) nil 1 (vector) 16 68  '(lambda (issue-list labeled)
                                                                                                                 (progn
                                                                                                                   (message "query worker arrange issue")
                                                                                                                   (carlos/gitlab-handle-issue-output issue-list labeled)
                                                                                                                   )))))))
(defun carlos/gitlab-mini-group-week-report (src-list)
  (let ((worker (car src-list))
        (left-workers (cdr src-list)))
    (let ((worker-label (cdr worker))
          (left-workers left-workers)
          )
      (setq carlos/gitlab-mini-group-week-report-bot-left-workers left-workers)
      (message "worker label is:%s left src list is:%s" worker-label (length left-workers))
      (carlos/gitlab_query_labeled_issue (car worker-label) nil 1 (vector) 16 68  '(lambda (issue-list labeled)
                                                                                     (progn
                                                                                       (message "handle work labels")
                                                                                       (carlos/gitlab-handle-issue-output issue-list labeled)

                                                                                       (let ((left-workers carlos/gitlab-mini-group-week-report-bot-left-workers))
                                                                                         (if (< 0 (length left-workers))
                                                                                             (carlos/gitlab-mini-group-week-report left-workers)
                                                                                           (message "Fetch End")))))))))

(defun carlos/gitlab-add-milestone (argv )
  (setq carlos/gitlab-new-milestone-id argv)
  (message "Querying Added milestones...")
  (request
   (format (concat carlos/gitlab-default-gitlaburl  "/projects/%s/milestones?state=active") argv)
   :type "GET"
   :data '(("per_page" . "99") ("order_by" . "updated_at") ("sort" . "desc"))
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq data (sort data (lambda (data1 data2)
                                       (time-less-p  (carlos/parse-time (cdr (assoc 'due_date data2))) (carlos/parse-time (cdr (assoc 'due_date data1))))
                                       )))
               (let ((sort_milestones '()))
                 (cl-loop for i across data
                          do(progn
                              (setq sort_milestones (append sort_milestones (list (cons (concat (decode-coding-string (cdr (assoc 'title i)) 'utf-8-emacs) " " (cdr (assoc 'due_date i))) (cdr (assoc 'id i))))))))
                 (setq carlos/debug/gitlab/cache/query-milestone-soruce sort_milestones)
                 (helm :sources (helm-build-sync-source "helm-gitlab-milestone"
                                  :candidates  sort_milestones
                                  :fuzzy-match t
                                  :action (helm-make-actions "Close Selected Issue"   (lambda (candidate)

                                                                                        candidate
                                                                                        )
                                                             "Open Select Issue"  (lambda (candidate)

                                                                                    candidate
                                                                                    )))
                       :input (carlos/gitlab-util-pure-milestone-title (org-get-heading nil t))
                       :buffer "*helm gitlab projects*"
                       )
                 (progn
                   (setq choose_due_date
                         (org-read-date nil nil nil nil nil (carlos/get-last-week-work-day (format-time-string "%Y-%m-%d" (time-add (carlos/parse-time (format-time-string "%Y-%m-%d")) (* 10 (* 60 60 24))))
                                                                                           (format-time-string "%Y-%m-%d" (time-add (carlos/parse-time (format-time-string "%Y-%m-%d")) (* 2 (* 60 60 24))))
                                                                                           "6" 1))
                         )
                   )
                 (setq argv1 (read-string "请输入里程碑的名称:" (carlos/org_get_milestone_full_outling_heading) ))
                 (setq carlos/gitlab-add-milestone-param (list (cons "id" carlos/gitlab-new-milestone-id ) (cons "title" argv1)
                                                               ;; (cons "due_date" choose_due_date)
                                                               ))
                 (carlos/invalid-gitlab-choose-milestone)
                 (message "发送里程碑信息给服务器...")
                 (request
                  (format (concat carlos/gitlab-default-gitlaburl "/projects/%s/milestones") carlos/gitlab-new-milestone-id)
                  :type "POST"
                  :data carlos/gitlab-add-milestone-param
                  :parser 'json-read
                  :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
                  :success (cl-function
                            (lambda (&key data &allow-other-keys)
                              (setq carlos/debug-gitlab-add-miles-http-result-data data)
                              (setq carlos/debug-gitlab-add-miles-http-result-argv argv1)
                              (message "添加里程碑成功 尝试自动添加里程碑关联的工单")
                              (carlos/gitlab-add-notify-to-milestone-include-issue argv1 data)
                              ))
                  :error (cl-function
                          (lambda (&key data &allow-other-keys)
                            (message "添加里程碑失败:%s" data)
                            )))
                 )))
   :error    (cl-function
              (lambda (&key data &allow-other-keys))
              )))

(defun carlos/gitlab-add-milestone-with-param ()
  "添加里程碑的函数"
  (interactive)
  (message "debug add mile with param")
  (setq carlos/gitlab-new-milestone-id (carlos/helm-choose-which-gitlabproject))
  (carlos/gitlab-query-milestone-for-add-issue carlos/gitlab-new-milestone-id (vconcat) 1 (carlos/gitlab-util-pure-milestone-title (org-get-heading nil t)) (lambda (choose-milestone projectid)
                                                                                                      (message "debug choose milestone is:%s" choose-milestone)
                                                                                                      ;; (message "carlos choose milestone is:%s not nil?%s" choose-milestone (not (equal nil choose-milestone)))
                                                                                                      (if (not (equal nil choose-milestone))
                                                                                                          (message "milestone:%s is Exist" choose-milestone)
                                                                                                        (progn
                                                                                                          (message "start send milestone")
                                                                                                          (setq argv1 (read-string "请输入里程碑的名称:" (carlos/org_get_milestone_full_outling_heading) ))
                                                                                                          (message "get argv1 is:%s" argv1)
                                                                                                          (setq carlos/gitlab-add-milestone-param (list (cons "id" carlos/gitlab-new-milestone-id ) (cons "title" argv1)))
                                                                                                          ;; (carlos/invalid-gitlab-choose-milestone)
                                                                                                          (message "发送里程碑信息给服务器...")
                                                                                                          (request
                                                                                                           (format (concat carlos/gitlab-default-gitlaburl "/projects/%s/milestones") carlos/gitlab-new-milestone-id)
                                                                                                           :type "POST"
                                                                                                           :data carlos/gitlab-add-milestone-param
                                                                                                           :parser 'json-read
                                                                                                           :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
                                                                                                           :success (cl-function
                                                                                                                     (lambda (&key data &allow-other-keys)
                                                                                                                       (setq carlos/debug-gitlab-add-miles-http-result-data data)
                                                                                                                       (setq carlos/debug-gitlab-add-miles-http-result-argv argv1)
                                                                                                                       (message "添加里程碑成功 尝试自动添加里程碑关联的工单")
                                                                                                                       (carlos/gitlab-add-notify-to-milestone-include-issue argv1 data)
                                                                                                                       ))
                                                                                                           :error (cl-function
                                                                                                                   (lambda (&key data &allow-other-keys)
                                                                                                                     (message "添加里程碑失败:%s" data)
                                                                                                                     ))))
                                                                                                        )
                                                                                                      ))
  ;; (carlos/gitlab-add-milestone (carlos/helm-choose-which-gitlabproject))
  )

;; (org-read-date nil nil nil  "4/20" "+2d")

(defun carlos/create-gitlab-project-issue-list (page_index page_concat callback)
  (setq carlos/gitlab-call-http-request t)
  (setq cur_page_index (+ page_index 1))
  (setq cur_page_concat page_concat)
  (setq callback callback)
  (if (equal carlos/gitlab-call-http-request t)
      (progn

        (request
         (concat carlos/gitlab-default-gitlaburl "/projects/" carlos/gitlab-projectid "/issues")
         :type "GET"
         :parser 'json-read
         :data (list (cons "page" cur_page_index) (cons "per_page"  90))
         :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
         :success (cl-function
                   (lambda (&key data &allow-other-keys)
                     ;; (setq carlos-item (aref data 0))
                     ;; (setq carlos-issue-content (cdr (assq 'title carlos-item)))
                     (setq cur_page_concat (vconcat cur_page_concat data))
                     (message "Fetch Page:%s" cur_page_index)
                     (cond ((>= (length data) 90)
                            (carlos/create-gitlab-project-issue-list cur_page_index cur_page_concat callback)
                            )
                           (t
                            (setq search-source '())
                            (cl-loop for i across cur_page_concat
                                     do (progn
                                          (setq converted-item "")
                                          (cl-loop for j across (cdr (assq 'labels i))
                                                   do (progn
                                                        (setq converted-item (concat (decode-coding-string j 'utf-8-emacs) " " converted-item))
                                                        ))
                                          (setq converted-item (concat converted-item (replace-regexp-in-string "\n" "" (concat (cdr (assq 'state i))  " " (decode-coding-string (cdr (assq 'title i)) 'utf-8-emacs)))))
                                          (setq search-source
                                                (append search-source
                                                        (list (cons converted-item (list  (cdr (assq 'web_url i)) (cdr (assq 'state i))) ))))))
                            (setq carlos/week-done-labeled-searching t)
                            (setq carlos/gitlab-week-report-last-pint (point))
                            (org-map-entries
                             (lambda ()
                               (recenter)
                               (cond ((and  (equal t carlos/week-done-labeled-searching) (org-get-heading t t))
                                      (progn
                                        (setq carlos/debug-auto-bot/search-source search-source)
                                        ;; (setq carlos/debug-auto-bot/current-heading (carlos/gitlab-filter-special-char (org-get-heading t t)))
                                        (setq carlos/debug-auto-bot/current-heading (org-get-heading t t))
                                        (helm :sources (helm-build-sync-source "get working entry "
                                                         :candidates search-source
                                                         :fuzzy-match t
                                                         :action (helm-make-actions "Close Selected Issue"   (lambda (candidate)
                                                                                                               (message "choose candidate is:%s" candidate)
                                                                                                               (cond ((equal "closed" (nth 1 candidate))
                                                                                                                      (org-todo "DONE")
                                                                                                                      ))
                                                                                                               ;; (message "debug choose item")
                                                                                                               t
                                                                                                               )
                                                                                    "Open Select Issue"  (lambda (candidate)
                                                                                                           (message "debug open select issue :%s" candidate)
                                                                                                           ;; (setq carlos/week-done-labeled-searching nil)
                                                                                                           (browse-url (car candidate))
                                                                                                           t
                                                                                                           )
                                                                                    "Quit week report loop"  (lambda (candidate)
                                                                                                               (setq carlos/week-done-labeled-searching nil)
                                                                                                               (setq carlos/gitlab-week-report-last-pint (point))
                                                                                                               t
                                                                                                               )
                                                                                    "Try to reset to TODO" (lambda (candidate) (interactive "")
                                                                                                             (message "debug try to reset to todo:%s" candidate)
                                                                                                             (org-todo "TODO")
                                                                                                             )

                                                                                    ))
                                              :input (carlos/gitlab-filter-special-char (org-get-heading t t))
                                              :buffer "*Query Weekly Issue Report*")))))
                             "+TODO=\"LABELED\"" (list (buffer-file-name (current-buffer))))
                            ;; (message "after handle:%s" carlos/gitlab-projectid)
                            (goto-char carlos/gitlab-week-report-last-pint)
                            (funcall  callback carlos/week-done-labeled-searching)))))
         :error (cl-function
                 (lambda (&key data &allow-other-keys)
                   (message "Get Project Issues fail : %S" data)))))))

(defun carlos/gitlab-week-report-bot (&optional report-bot-callback)
  (interactive)
  (setq carlos/gitlab-projectid (carlos/helm-choose-which-gitlabproject))
  (message "Start Sync Project Status..." )
  (carlos/create-gitlab-project-issue-list 0 (vector) (lambda (item) (interactive "")
                                                        (if report-bot-callback
                                                            (funcall report-bot-callback))
                                                        )))

(defun carlos/org_get_full_outling_heading ()
  (message "%s" (concat (mapconcat 'identity (org-get-outline-path) " ")  " " (org-get-heading t t) )))

(defun carlos/org_get_milestone_full_outling_heading ()
  "获得当前 heading 的完整路径做为 milestone"
  (interactive "")
  (message "%s" (concat (mapconcat 'identity (org-get-outline-path) " ")  " " (carlos/gitlab-util-pure-milestone-title (org-get-heading nil t))  )))

(defun carlos/org-get-parent-heading ()
  (or (car (last (org-get-outline-path))) " "))

(defun carlos/parse-time-zone (timestr)
  (let ((time (timezone-parse-date timestr))
        )
    (let ((hour-min-sec (parse-time-string (elt time 3))))
      (encode-time (nth 0 hour-min-sec) (nth 1 hour-min-sec) (nth 2 hour-min-sec) (string-to-number (elt time 2)) (string-to-number (elt time 1)) (string-to-number (elt time 0)))
      )
    ))

(defun carlos/parse-time (timestr)
  (let ((time1 (parse-time-string (or  timestr (format-time-string "%Y-%m-%d")))))
    (encode-time 0 0 0 (nth 3 time1) (nth 4 time1) (nth 5 time1))
    ))

(defun carlos/get-last-week-work-day (argv today target_date target_work_date_count)
  (let (
        ;; (today (format-time-string "%Y-%m-%d"))
        ;; (target_date "Fri")
        (loop_date argv)
        (get_date (format-time-string "%Y-%m-%d"))
        (work-due-date '())
        (found_count 0)
        (looping t)
        )
    (while (and (not (string= loop_date today)) looping t)
      ;; (message "loop date is:%s today is:%s loop data week index is:%s target_date is:%s" loop_date today (format-time-string "%u" (carlos/parse-time loop_date)) target_date)
      (cond ((string= target_date (format-time-string "%u" (carlos/parse-time loop_date)))
             (message "get target date:%s" loop_date)
             (setq get_date loop_date)
             (cond ((>= found_count target_work_date_count)
                    (setq looping nil)
                    ))
             (setq found_count (+ 1 found_count))
             ))
      (setq loop_date (format-time-string "%Y-%m-%d" (time-add (carlos/parse-time loop_date) (- 0 (* 60 60 24))))))
    get_date
    ))

(defun carlos/gitlab-add-dev-issue-with-project-milestone ()
  (let ((target-gitlab-project-id (carlos/helm-choose-which-gitlabproject)))
    (carlos/query-milestone target-gitlab-project-id '(lambda (choose-milestone-id project-id)
                                                        (progn
                                                          (carlos/gitlab-add-issue choose-milestone-id project-id "Sat" 1))))))

(defun carlos/query-milestone (argv callback)
  (message "Querying Milestones...")
  (if (carlos/parse-heading-is-fix-qa (org-get-heading t t))
      (progn
        (funcall callback nil argv)
        )
    (progn
      (let ((argv (or argv -1))
            (cache_milestone (cdr (assoc argv carlos/gitlab-query-last-milestone)))
            )
            ;; (cache_milestone_timestamp (cdr (assoc argv carlos/gitlab-query-last-milestone-timestamp))))
        (setq carlos/gitlab-milestone-choose-project-id argv)
        (message "query milestone argv is:%s" argv)
        (message "debug cache milestone is equal nil:%s" (equal nil cache_milestone))
        (if nil ;;(equal argv -1)
            (progn
              (carlos/gitlab-add-issue  nil nil))
          (if nil ;;(not (equal nil cache_milestone))
              (progn

                (setq choose-milestone (helm :sources (helm-build-sync-source "helm-gitlab-milestone"
                                                        :candidates  cache_milestone
                                                        :fuzzy-match t
                                                        :action (helm-make-actions "Close Selected Issue"   (lambda (candidate)
                                                                                                              (message "choose milestone  is:%s" candidate)
                                                                                                              candidate
                                                                                                              )
                                                                                   "Open Select Issue"  (lambda (candidate)

                                                                                                          candidate
                                                                                                          )
                                                                                   ))
                                             :input (carlos/org-get-parent-heading)
                                             :buffer "*helm gitlab projects*" ))
                ;; (carlos/gitlab-add-issue  choose-milestone carlos/gitlab-milestone-choose-project-id)
                (funcall callback choose-milestone carlos/gitlab-milestone-choose-project-id)
                )
            (progn
              (setq carlos/gitlab-milestone-query-callback callback)
              ;; (message "start call carlos/gitlab-query-milestone-for-add-issue" )
              (carlos/gitlab-query-milestone-for-add-issue argv (vconcat) 1 (carlos/org-get-parent-heading) (lambda (choose-milestone carlos/gitlab-milestone-choose-project-id)
                                                                            (if (equal nil choose-milestone)
                                                                                (message "Fetch milestone fail" )
                                                                              (funcall carlos/gitlab-milestone-query-callback choose-milestone carlos/gitlab-milestone-choose-project-id)))))
            ))))))

(defun carlos/gitlab-milestone-parser (milestones)
  (setq data (sort milestones (lambda (data1 data2)
                                ;; (message "debug gitlab milestone parser data1:%s data2:%s" data1 data2)
                          (time-less-p  (carlos/parse-time (cdr (assoc 'due_date data2)) ) (carlos/parse-time (cdr (assoc 'due_date data1))))
                          )))
  (let ((sort_milestones '()))
    (cl-loop for i across data
             do(progn
                 (setq sort_milestones (append sort_milestones (list (cons (decode-coding-string (cdr (assoc 'title i)) 'utf-8-emacs) (list (cdr (assoc 'iid i)) (cdr (assoc 'due_date i)) (cdr (assoc 'id i)) (cdr (assoc 'state i)) (cdr (assoc 'updated_at i)) )))))))
    sort_milestones))

(defun carlos/gitlab-query-milestone (argv last-milestones target-gitlab-project cur_page_index)
  (let ((target-gitlab-project target-gitlab-project))

    (setq carlos/gitlab-query-milestone-targetproject target-gitlab-project)
    (setq carlos/gitlab-query-milestone-heading argv)
    (setq carlos/gitlab-queried-milestones last-milestones)
    (setq carlos/gitlab-milestone-choose-project-url (car target-gitlab-project))
    (setq carlos/gitlab-query-milestone-title argv)
    (setq carlos/gitlab-query-milestone-page-index cur_page_index)

    (message "Last Queried milestone length is:%s" (length last-milestones))
    (request
     (format (concat carlos/gitlab-default-gitlaburl  "/projects/%s/milestones") (cdr target-gitlab-project))
     :type "GET"
     :parser 'json-read
     :data (list (cons "order_by"  "updated_at") (cons "sort"  "desc") (cons "page" cur_page_index) (cons "per_page"  90))
     :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Data length is:%s typeof is:%s cond is:%s" (length data) (type-of data) (< 10 (length data)))
                 (cond ((> 90 (length data))
                        (progn
                          (message "show helm query milestone")
                          (setq carlos/gitlab-milestone-web-url-format (concat "http://112.74.81.51:10088/" carlos/gitlab-milestone-choose-project-url "/milestones/%s"))
                          (message "Get MileStones url:%s" carlos/gitlab-milestone-choose-project-url)
                          (setq carlos/debug-gitlab-debug-milestone-data-list data)

                          (helm :sources (helm-build-sync-source "*helm gitlab choose projects*"
                                           :candidates  (carlos/gitlab-milestone-parser (vconcat (or carlos/gitlab-queried-milestones (vconcat)) data))
                                           :fuzzy-match t
                                           :action  (helm-make-actions "Opened select milestone"  (lambda (candidate)
                                                                                                    (message "choose milestone  is:%s" candidate)
                                                                                                    (browse-url (format carlos/gitlab-milestone-web-url-format (car candidate)))
                                                                                                    candidate
                                                                                                    )
                                                                       "Opened select milestone"  (lambda (candidate)

                                                                                                    (browse-url (format carlos/gitlab-milestone-web-url-format (car candidate)))
                                                                                                    candidate
                                                                                                    )
                                                                       )
                                           )
                                :input carlos/gitlab-query-milestone-title
                                :buffer "*helm gitlab choose projects*" )))
                       (t
                        (progn
                          (message " data is:%s" (type-of data))
                          (message "last milestone is:%s" (type-of  carlos/gitlab-queried-milestones))

                          (carlos/gitlab-query-milestone carlos/gitlab-query-milestone-heading (vconcat (or carlos/gitlab-queried-milestones (vconcat)) data)  carlos/gitlab-query-milestone-targetproject (+ 1 carlos/gitlab-query-milestone-page-index)))))))
     :error (cl-function
             (lambda (&key data &allow-other-kesy)
               (message "Query milestone data fail:%s" data))))))

(defun carlos/gitlab-util-pure-milestone-title (milestone-title)
  (message "debug pure mile stone title is:%s" milestone-title)
  (replace-regexp-in-string "\\[[0-9]+\\/[0-9]+\\]" "" milestone-title))

(defun carlos/gitlab-query-milestone-with-current-heading ()
  (interactive )
  (let ((cur-heading (carlos/gitlab-util-pure-milestone-title (org-get-heading t t)) ))
    (message "try to query milestone")
    (carlos/gitlab-query-milestone cur-heading  (vconcat) (carlos/gitlab-choose-gitlabproject) 1)))

(defun carlos/gitlab-query-milestone-after-date (target-date)
  (carlos/gitlab-query-milestone-bypagesize 2)
  )

(defvar carlos/gitlab-milestone-report-buffer-name "*Milestone Report*" )

(defun carlos/gitlab-query-milestone-bypagesize (target-gitlab-project-list target-date)
  (let ((target-gitlab-project (cdr (car target-gitlab-project-list))))
    (message "target-gitlab-project is:%s" target-gitlab-project)
    (message "Fetching milestones data target date is:%s" target-date)

    (setq carlos/gitlab-left-milestone-project-list (cdr target-gitlab-project-list))
    (setq carlos/gitlab-milestone-choose-project-url (car target-gitlab-project))
    (setq carlos/gitlab-query-milestone-title argv)
    (setq carlos/choose-query-milestone-target-date target-date)
    (request
     (format (concat carlos/gitlab-default-gitlaburl  "/projects/%s/milestones") (cdr target-gitlab-project))
     :type "GET"
     :parser 'json-read
     :data '(("per_page" . "99") ("order_by" . "updated_at") ("sort" . "desc"))
     :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Get milestone data left-milestone-project-list size is:%s" (length carlos/gitlab-left-milestone-project-list))
                 (cond ((< 0 (length carlos/gitlab-left-milestone-project-list))
                        (progn
                          (let ((milestone-list (carlos/gitlab-milestone-parser data)))

                            (setq carlos/gitlab-query-milestone-ret (append carlos/gitlab-query-milestone-ret  milestone-list))

                            (carlos/gitlab-query-milestone-bypagesize carlos/gitlab-left-milestone-project-list carlos/choose-query-milestone-target-date))))
                       (t
                        (progn
                          (message "try to output report Done Milestone")
                          (let ((milestone-list (carlos/gitlab-milestone-parser data))
                                )
                            (setq milestone-list (append carlos/gitlab-query-milestone-ret  milestone-list))
                            (setq carlos/gitlab-filtered-milestones (-filter (lambda (data)
                                                                               (progn
                                                                                 (time-less-p (carlos/parse-time carlos/choose-query-milestone-target-date) (carlos/parse-time (replace-regexp-in-string "T.*" "" (nth (- (length data) 1) data))))))
                                                                             milestone-list))
                            (setq carlos/gitlab-done-milestones (sort carlos/gitlab-filtered-milestones (lambda (data1 data2)
                                                                                                          (time-less-p  (carlos/parse-time (replace-regexp-in-string "T.*" "" (car (last data2))))
                                                                                                                        (carlos/parse-time (replace-regexp-in-string "T.*" "" (car (last data1)) ))))))
                            (setq carlos/gitlab-done-milestones (-filter (lambda (data)
                                                                           (progn
                                                                             (string= "closed" (nth (- (length data) 2) data))))
                                                                         carlos/gitlab-done-milestones))

                            ;; (with-output-to-temp-buffer "*Milestone Report*"
                            (print (format "MileStone Finished:%s" (length carlos/gitlab-done-milestones))   (get-buffer-create carlos/gitlab-milestone-report-buffer-name))
                            (mapc (lambda (data)
                                    (print (format "%s %s"  (car data) (car (last data))) (get-buffer-create carlos/gitlab-milestone-report-buffer-name)))
                                  carlos/gitlab-done-milestones)
                            ;; )
                            (setq carlos/gitlab-filtered-plan-milestones (sort carlos/gitlab-filtered-milestones (lambda (data1 data2)
                                                                                                                   (time-less-p  (carlos/parse-time (replace-regexp-in-string "T.*" "" (nth 2 data1)
                                                                                                                                                                              ))
                                                                                                                                 (carlos/parse-time (replace-regexp-in-string "T.*" "" (nth 2 data2) ))))))
                            (setq carlos/gitlab-filtered-plan-milestones (-filter (lambda (data)
                                                                                    (progn
                                                                                      (not (string= "closed" (nth (- (length data) 2) data)))))
                                                                                  carlos/gitlab-filtered-plan-milestones))
                            (print (format "MileStone Plan:%s" (length carlos/gitlab-filtered-plan-milestones))  (get-buffer-create carlos/gitlab-milestone-report-buffer-name))
                            (mapc (lambda (data)

                                    (print (format "%s %s due_date:%s"  (car data) (car (last data)) (nth 2 data) ) (get-buffer-create carlos/gitlab-milestone-report-buffer-name)))
                                  carlos/gitlab-filtered-plan-milestones
                                  )))))))
     :error (cl-function
             (lambda (&key data &allow-other-kesy)
               (message "Query milestone data fail:%s" data)
               )))))

(defun carlos/gitlab-aelos1s-milestone-report (project-lists)
  (let ((target-date (org-read-date nil nil nil nil nil (carlos/get-last-week-work-day (format-time-string "%Y-%m-%d") "2016-01-01" "7" 1))))
    (setq carlos/gitlab-query-milestone-ret '( ))
    (cond ((not (equal nil (get-buffer carlos/gitlab-milestone-report-buffer-name)))
           (kill-buffer carlos/gitlab-milestone-report-buffer-name)
           ))
    (carlos/gitlab-query-milestone-bypagesize project-lists target-date)
    (switch-to-buffer carlos/gitlab-milestone-report-buffer-name)))

(defun carlos/gitlab-milestone-report-bot ()
  (carlos/gitlab-aelos1s-milestone-report gitlab-project-milestone-report))

(defun carlos/gitlab-get-next-saturday (start-date)
  (let ((start-date start-date)
        (loop-date  start-date)
        (monday-find nil)
        (found-date "")
        (loop t)
        )
    (while (and loop t)
      (message "loop-date is:%s monday-find is:%s" loop-date monday-find)
      (setq loop-date (format-time-string "%Y-%m-%d" (time-add (carlos/parse-time loop-date) (+ 0 (* 60 60 24)))))
      (cond ((string= "Mon" (format-time-string "%a" (carlos/parse-time loop-date)))
             (setq monday-find t)
             ))
      (cond ((and (equal monday-find t) (string= "Sat" (format-time-string "%a" (carlos/parse-time loop-date))))
             (setq loop nil)
             (setq found-date loop-date))))
    found-date ))

(defun carlos/gitlab-filter-special-char (src-str)
  (let ((ret (replace-regexp-in-string "[][\\“\\”\\\"\\\\+-]" " " src-str)))
    (message "debug filter special char is:%s" ret)
    ret
    ))

;; (carlos/gitlab-filter-special-char "test+-123")

(setq carlog/gitlab-query-qa-bug-issue-pagesize 90)

(setq carlos/gitlab-projects-issue-offset 0)

;; (carlos/gitlab-filter-QA-not-confirm-issue calros/debug/gitlab-qa-data)

(defun carlos/gitlab-filter-QA-not-confirm-issue (data)
  (let ((author (cdr (assoc 'username  (assoc 'author data))))
        (labels (cdr (assoc 'labels data)))
        (assigne (cdr  (assoc 'username  (cdr  (assoc 'assignee data)))))
        (detected t)
        )

    (mapcar '(lambda (label)
               (if (or (string-equal "not-softdev" label) (string-equal "softdev-notplan" label))
                   (setq detected nil))
               ) labels)
    (if (or
         (-contains? carlos/gitlab-QA-filter-assign-list assigne)
         )
        (progn
          (setq detected t)
          )
      (progn
        (setq detected nil)
        )
      )
    detected
    )
  )

(defun carlos/gitlab-sort-issues-by-special-label (src-list label)
  "docstring"
  (setq  src-list (sort
                   src-list
                   '(lambda (a b)
                      (let ((a_labels (append (cdr (assoc 'labels a)) nil))
                            (b_labels (append (cdr (assoc 'labels b)) nil))
                            )
                        ;; (message "debug a_labels of label is:%s contains labels is:%s" a_labels label)
                        (setq a_labels (mapcar (lambda (item)
                                                 ;; (message "debug convert label is:%s" (decode-coding-string item 'utf-8-emacs))
                                                 (decode-coding-string item 'utf-8-emacs)
                                                 )
                                               a_labels))
                        (setq b_labels (mapcar (lambda (item)
                                                 ;; (message "debug convert label is:%s" (decode-coding-string item 'utf-8-emacs))
                                                 (decode-coding-string item 'utf-8-emacs)
                                                 )
                                               b_labels))

                        (cond
                         ((-contains? a_labels label) (setq a_labels 10))
                         (t (setq a_labels 0))
                         )
                        (cond
                         ((-contains? b_labels label) (setq b_labels 10))
                         (t (setq b_labels 0))
                         )
                        ;; (message "sort issue by label a_labels is:%s b_lables is:%s" a_labels b_labels)
                        (> a_labels b_labels))))))

(defun carlos/gitlab-sort-issues-by-created (src-list)
  "docstring"
  (setq  src-list (sort
                   src-list
                   '(lambda (a b)
                      (let ((a_labels (cdr (assoc 'created_at a)))
                            (b_labels (cdr (assoc 'created_at b)))
                            )
                        (< 0 (float-time (time-subtract
                                     (carlos/parse-time (xah-fix-datetime-stamp a_labels))
                                     (carlos/parse-time (xah-fix-datetime-stamp b_labels))))))))))

(defun carlos/gitlab-issue-filter (carlos/gitlab-qa-bug-issues-list carlos/gitlab-qa-bug-plan-or-not)
  "docstring"
  (interactive "P")
  (seq-filter (lambda (data)
                (progn
                  (setq calros/debug/gitlab-qa-data data)

                  (setq carlos/gitlab-qa-bug-filter-issue (not carlos/gitlab-qa-bug-plan-or-not))
                  (setq carlos/gitlab-qa-bug-filter-issue-fixed t)
                  (mapcar '(lambda (label)

                             (cond ((or (string-equal "已安排开发" (decode-coding-string label 'utf-8-emacs))
                                        (string-equal "产品软件已安排开发" (decode-coding-string label 'utf-8-emacs))
                                        (string-equal "已反馈" (decode-coding-string label 'utf-8-emacs))
                                        (string-equal "product-dev-arrange" (decode-coding-string label 'utf-8-emacs))
                                        (string-equal "product-fixed" (decode-coding-string label 'utf-8-emacs))
                                        (string-equal "softdev-in-process" (decode-coding-string label 'utf-8-emacs))
                                        (string-equal "softdev-fixed" (decode-coding-string label 'utf-8-emacs))
                                        )
                                    (setq carlos/gitlab-qa-bug-filter-issue carlos/gitlab-qa-bug-plan-or-not)
                                    ))

                             (if (or  (string-equal "product-fixed" (decode-coding-string label 'utf-8-emacs))
                                      (string-equal "softdev-fixed" (decode-coding-string label 'utf-8-emacs)))
                                 (progn

                                   (setq carlos/gitlab-qa-bug-filter-issue-fixed nil)
                                   )
                               )
                             ) (cdr (assoc 'labels data)))

                  (and carlos/gitlab-qa-bug-filter-issue carlos/gitlab-qa-bug-filter-issue-fixed (carlos/gitlab-filter-QA-not-confirm-issue data)) ))
              carlos/gitlab-qa-bug-issues-list))

(defun carlos/gitlab-queried-project-issues (data carlos/gitlab-qa-bug-project-list carlos/gitlab-qa-bug-plan-or-not)
  "docstring"
  (progn
    (setq carlos/gitlab-qa-bug-issues-list (vconcat carlos/gitlab-qa-bug-issues-list data))
    (setq carlos/gitlab-qa-bug-project-list (cdr carlos/gitlab-qa-bug-project-list))
    (cond ((< 0 (length carlos/gitlab-qa-bug-project-list))
           (progn

             (carlos/gitlab-projecst-issues carlos/gitlab-qa-bug-project-list 1 carlos/gitlab-qa-bug-issues-list carlos/gitlab-qa-bug-plan-or-not)
             )
           )
          (t
           (progn
             (message "Try to filter issue list")
             (setq carlos/gitlab-qa-bug-filter-issue-list  (carlos/gitlab-issue-filter carlos/gitlab-qa-bug-issues-list carlos/gitlab-qa-bug-plan-or-not))
             (message "debug gitlab project issue offset is:%s length of src list is:%s" carlos/gitlab-projects-issue-offset (length carlos/gitlab-qa-bug-filter-issue-list))
             (if (< (length carlos/gitlab-qa-bug-filter-issue-list) carlos/gitlab-projects-issue-offset)
                 (progn
                   (setq carlos/gitlab-projects-issue-offset  0)
                   (terminal-notifier-notify "读取项目issue 列表提醒" "列表已经读取完，从头开始" "org.hi")
                   )
               (progn
                 (setq carlos/gitlab-qa-bug-filter-issue-list
                       ;; (carlos/gitlab-sort-issues-by-created
                       (carlos/gitlab-sort-issues-by-special-label
                        (carlos/gitlab-sort-issues-by-created carlos/gitlab-qa-bug-filter-issue-list)
                        "严重问题"))
                 (cond ((equal nil carlos/gitlab-qa-bug-plan-or-not)
                        (progn
                          (get-buffer-create "* QA Bug-Reprot *")
                          (kill-buffer "* QA Bug-Reprot *")
                          (switch-to-buffer "* QA Bug-Reprot *")
                          (org-mode)
                          (insert "* IssueList [0/0]")
                          (mapcar '(lambda (data)
                                     ;; (message "data is:%s" (cdr (assoc 'labels data)))
                                     (setq show-labels ":")
                                     (mapcar '(lambda (label)
                                                (interactive)
                                                ;; (message "debug label is:%s" (decode-coding-string label 'utf-8-emacs))
                                                (setq show-labels (concat show-labels  (decode-coding-string label 'utf-8-emacs) ":" ))
                                                ;; (message "after update show-label")
                                                )
                                             (cdr (assoc 'labels data)))
                                     (setq carlos/gitlab-qa-bug-print-issue (format "\n** TODO 处理 %s %s %s\n author:%s \n assigne:%s  \n\"%s\""
                                                                                    (and (cdr (assoc 'web_url data)) (decode-coding-string (cdr (assoc 'web_url data)) 'utf-8-emacs))
                                                                                    (and (cdr (assoc 'title data)) (decode-coding-string (cdr (assoc 'title data)) 'utf-8-emacs))
                                                                                    show-labels
                                                                                    (cdr  (assoc 'name  (cdr  (assoc 'author data))))
                                                                                    (cdr  (assoc 'username  (cdr  (assoc 'assignee data))))
                                                                                    ;; (and (cdr (assoc 'author data)) (decode-coding-string (cdr (assoc 'author data)) 'utf-8-emacs))
                                                                                    ;; (and (cdr (assoc 'labels data)) (decode-coding-string (cdr (assoc 'labels data)) 'utf-8-emacs))
                                                                                    (and (cdr (assoc 'description data)) (replace-regexp-in-string "[*\"]" ""  (decode-coding-string (cdr (assoc 'description data)) 'utf-8-emacs)))
                                                                                    ))
                                     (if (and carlos/gitlab-qa-bug-plan-or-not (cdr (assoc 'web_url data)))
                                         (browse-url (decode-coding-string (cdr (assoc 'web_url data)) 'utf-8-emacs)))
                                     (insert carlos/gitlab-qa-bug-print-issue))
                                  carlos/gitlab-qa-bug-filter-issue-list)
                          ;; (goto-char 0)
                          ;; (org-update-statistics-cookies)
                          (org-update-parent-todo-statistics)
                          ;; (insert "\n* done")
                          )
                        )
                       (t

                        (message "debug qa bug fitler src list length is:%s" (length  carlos/gitlab-qa-bug-filter-issue-list))
                        (mapcar '(lambda (data)
                                   (setq carlos/gitlab-qa-bug-print-issue (format "\n%s \n"
                                                                                  (and (cdr (assoc 'web_url data)) (decode-coding-string (cdr (assoc 'web_url data)) 'utf-8-emacs))))
                                   (message "after gitlab qa debug planed issue"))
                                carlos/gitlab-qa-bug-filter-issue-list)
                        (if (and carlos/gitlab-qa-bug-filter-issue-list (< 0 (length carlos/gitlab-qa-bug-filter-issue-list)))
                            (carlos/gitlab-debug-QA-planed-issues carlos/gitlab-qa-bug-filter-issue-list)
                            ))))))))))

(defun carlos/gitlab-Query-projects-list-issue (project-list cur_page_index page_size history callback)
  "docstring"
  (interactive "P")
  (let ((project-id (cdr  (cdr (car project-list))))
        (left-project-lists project-list)
        )
    (request
     (concat carlos/gitlab-default-gitlaburl "/projects/" project-id "/issues")
     :type "GET"
     :parser 'json-read
     :data (list (cons "page" cur_page_index) (cons "per_page"  page_size) (cons "state" "opened"))
     :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Get Project Issuses project id:%s get data length:%s Cur page:%s" carlos/gitlab-project-issues-projectid (length data) cur_page_index)
                 (setq history (vconcat history data))
                 (cond ((> page_size (length data))
                        (progn
                          (setq left-project-lists (car project-list))
                          (if (< 0 (length left-project-lists))
                              (progn
                                (carlos/gitlab-Query-projects-list-issue left-project-lists 1 page_size history callback)
                                )
                            (progn
                              (funcall callback history))
                              )
                          )
                        ;; (carlos/gitlab-queried-project-issues data carlos/gitlab-qa-bug-project-list plan-or-not)
                        )
                       (t
                        (progn
                          (carlos/gitlab-Query-projects-list-issue left-project-lists (+ 1 cur_page_index) page_size history callback))))))
     :error (cl-function
             (lambda (&key data &allow-other-kesy)
               (message "query issue error %s" data))))))

(defun carlos/gitlab-projecst-issues (project-list cur_page_index result-issues plan-or-not)
  (let ((project-id (cdr  (cdr (car project-list))))
        )
    (setq carlos/gitlab-project-issues-projectid project-id)

    (setq carlos/gitlab-qa-bug-issues-list result-issues)
    (setq carlos/gitlab-qa-bug-project-list project-list)
    (setq carlos/gitlab-qa-bug-cur-page-index cur_page_index)
    (setq carlos/gitlab-qa-bug-last-project-id project-id)
    (setq carlos/gitlab-qa-bug-plan-or-not plan-or-not)
    (request
     (concat carlos/gitlab-default-gitlaburl "/projects/" carlos/gitlab-project-issues-projectid "/issues")
     :type "GET"
     :parser 'json-read
     :data (list (cons "page" cur_page_index) (cons "per_page"  carlog/gitlab-query-qa-bug-issue-pagesize) (cons "state" "opened"))
     :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Get Project Issuses project id:%s get data length:%s Cur page:%s" carlos/gitlab-project-issues-projectid (length data) cur_page_index)

                 (cond ((> carlog/gitlab-query-qa-bug-issue-pagesize (length data))
                        (carlos/gitlab-queried-project-issues data carlos/gitlab-qa-bug-project-list plan-or-not)
                        )
                       (t
                        (progn
                          (setq carlos/gitlab-qa-bug-issues-list (vconcat carlos/gitlab-qa-bug-issues-list data))

                          (carlos/gitlab-projecst-issues carlos/gitlab-qa-bug-project-list
                                                         (+ 1 carlos/gitlab-qa-bug-cur-page-index)
                                                         carlos/gitlab-qa-bug-issues-list
                                                         carlos/gitlab-qa-bug-plan-or-not))))))
     :error (cl-function
             (lambda (&key data &allow-other-kesy)
               (message "query issue error %s" data))))))

(defun carlos/gitlab-convert-label-to-score (label)
  "docstring"
  (interactive )
  (let ((score 0))
    (cond
     ((carlos/gitlab-is-include-label label "严重问题") (setq score 10))
     ((carlos/gitlab-is-include-label label "softdev-plan-created") (setq score 9))
     ((carlos/gitlab-is-include-label label "softdev-accept") (setq score 8))
     )
    score
    )
  )

(defun carlos/gitlab-sort-issues-by-planed-created-label (src-list)
  "docstring"
  (setq  src-list (sort
                   src-list
                   '(lambda (a b)
                      (let ((a_labels (append (cdr (assoc 'labels a)) nil))
                            (b_labels (append (cdr (assoc 'labels b)) nil))
                            )
                        (setq a_labels (carlos/gitlab-convert-label-to-score a_labels))
                        (setq b_labels (carlos/gitlab-convert-label-to-score b_labels))
                        (> a_labels b_labels))))))

(defun carlos/gitlab-sort-issues-by-created-timestamp (src-list)
  "docstring"
  (interactive)
  (setq src-list (sort src-list '(lambda (a b)
                                   (let ((a-time (parse-iso8601-time-string (cdr (assoc 'updated_at a))) )
                                         (b-time (parse-iso8601-time-string (cdr (assoc 'updated_at b))) )
                                         )
                                     ;; (message "Updated at is:%s" (assoc 'updated_at a))
                                     (time-less-p a-time b-time )))))
  src-list )

(defun carlos/gitlab-sort-issues-by-priority-lables (src-list)
  "docstring"
  (setq  src-list (sort
                   src-list
                   '(lambda (a b)
                      ;; (interactive "")

                      (let ((a_labels (append (cdr (assoc 'labels a)) nil))
                            (b_labels (append (cdr (assoc 'labels b)) nil))
                            )
                        (cond
                         ((carlos/gitlab-is-include-label a_labels "严重问题") (setq a_labels 10))
                         ((carlos/gitlab-is-include-label a_labels "P1") (setq a_labels 10))
                         ((carlos/gitlab-is-include-label a_labels "P2") (setq a_labels 9))
                         ((carlos/gitlab-is-include-label a_labels "P3") (setq a_labels 8))
                         ((carlos/gitlab-is-include-label a_labels "P4") (setq a_labels 7))
                         (t (setq a_labels 0))
                         )
                        (cond
                         ((carlos/gitlab-is-include-label b_labels "严重问题") (setq b_labels 10))
                         ((carlos/gitlab-is-include-label b_labels "P1") (setq b_labels 10))
                         ((carlos/gitlab-is-include-label b_labels "P2") (setq b_labels 9))
                         ((carlos/gitlab-is-include-label b_labels "P3") (setq b_labels 8))
                         ((carlos/gitlab-is-include-label b_labels "P4") (setq b_labels 7))
                         (t (setq b_labels 0))
                         )
                        (> a_labels b_labels)
                        ))))
  src-list
  )

(defun carlos/gitlab-debug-QA-planed-issues (issue-list)
  "docstring"
  (message "Left be handle QA issue list length is:%s" (length  issue-list))
  (let ((issue (car issue-list))
        (left-issues (cdr issue-list))
        )

    (let ((issue-iid (int-to-string (cdr (assoc 'iid issue))))
          (project-id (int-to-string (cdr (assoc 'project_id issue))))
          (web-url (cdr (assoc 'web_url issue)))
          )

      (carlos/gitlab-bot-auto-labeled-fixed-issue project-id issue-iid issue (lambda (data)
                                                                               (message "Carlos debug auto labeled fixed issue data:%s" data)
                                                                               (if (< 0 (length left-issues))
                                                                                   (progn
                                                                                     (carlos/gitlab-debug-QA-planed-issues left-issues)
                                                                                     )
                                                                                 (progn
                                                                                   (message "QA issue handle end")
                                                                                   )))))))

(defun carlos/gitlab-QA-inbox ()
  "用于读取出所有未安排的QA工单"
  ;; (interactive)
   (carlos/gitlab-projecst-issues gitlab-project-QA-BUG  1 (vector ) nil)
  )

(defun carlos/tool-gitlab-auto-Update-fixed-QA ()
  (interactive "")
  (carlos/gitlab-bot-auto-send-handled-info)
  )

(defun carlos/gitlab-bot-auto-send-handled-info ()
  "自动发送邮件通知已安排的QA已解决"
  (interactive)
  ;; (message "debug carlos/gitlab-bot-auto-send-handled-info")
  (carlos/gitlab-projecst-issues gitlab-project-QA-BUG  1 (vector ) t))

(defun carlos/gitlab-get-issue-notes (project_id issue_id)
  (request
   (concat carlos/gitlab-default-gitlaburl "/projects/" project_id "/issues/" issue_id "/notes")
   :type "GET"
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "get data length:%s" (length data))
               (mapcar  (lambda (item)
                          (message "dump item is:%s " item))
                        data)))
   :error (cl-function
           (lambda (&key data &allow-other-kesy)
             (message "get data fail %s" data)))))

(defun carlos/gitlab-get-merge-request-notes (token cur_page_index page_size project_id target_id  history-notes src-info finish-func)
  "docstring"
  (interactive )
  (funcall (curry 'carlos/gitlab-get-notes-all-by-type "/merge_requests/" token project_id target_id cur_page_index page_size )
           history-notes nil finish-func)
  )

(defun carlos/gitlab/leju-product-commitee-merget-bot/get-merge-request-notes (project_id target_id finish-func)
  "docstring"
  (interactive )
  (funcall (curry 'carlos/gitlab-get-merge-request-notes carlos/gitlab-approver-bot-token 1 90) project_id target_id (list) nil finish-func)
  )

(defun carlos/gitlab-get-notes-all-by-type (type token project_id target_id cur_page_index page_size history-notes src-info finish-func)
  (request
   (concat carlos/gitlab-default-gitlaburl "/projects/" project_id type target_id "/notes")
   :type "GET"
   :data (list (cons "page" cur_page_index) (cons "per_page" page_size) )
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  token))
   :success
   (cl-function
    (lambda (&key data &allow-other-keys)

      (if (< (length data) page_size)
          (progn
            (let ((total-history (vconcat history-notes data)))
              (funcall finish-func total-history src-info)
              )
            )
        (progn
          (carlos/gitlab-get-notes-all-by-type
           type
           token
           project_id
           issue_id
           (+ 1 cur_page_index)
           page_size
           (vconcat history-notes data)
           src-info
           finish-func
           )))
      ))
   :error (cl-function
           (lambda (&key data &allow-other-kesy)
             (funcall finish-func total-history src-info)
             (message "Get carlos/gitlab-get-notes-all-by-type Error: %s" data)))))

(defun carlos/gitlab-get-merge-request-info (token project_id target_id callback)
  "docstring"
  (interactive)
  (request
   (concat carlos/gitlab-default-gitlaburl "/projects/" project_id  "/merge_requests/" target_id)
   :type "GET"
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  token))
   :success
   (cl-function
    (lambda (&key data &allow-other-keys)
      (funcall callback data)))
   :error (cl-function
           (lambda (&key data &allow-other-kesy)
             (funcall callback nil)
             (message "Get carlos/gitlab-get-merge-request-info Error: %s" data)))))

(defun carlos/gitlab-get-issue-notes-all (project_id issue_id cur_page_index page_size history-notes src-info finish-func)
  (request
   (concat carlos/gitlab-default-gitlaburl "/projects/" project_id "/issues/" issue_id "/notes")
   :type "GET"
   :data (list (cons "page" cur_page_index) (cons "per_page" page_size) )
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success
   (cl-function
    (lambda (&key data &allow-other-keys)

      (if (< (length data) page_size)
          (progn
            (let ((total-history (vconcat history-notes data)))
              (funcall finish-func total-history src-info)
              )
            )
        (progn

          (carlos/gitlab-get-issue-notes-all project_id
                                             issue_id
                                             (+ 1 cur_page_index)
                                             page_size
                                             (vconcat history-notes data)
                                             src-info
                                             finish-func
                                             )))
      ))
   :error (cl-function
           (lambda (&key data &allow-other-kesy)
             (funcall finish-func total-history src-info)
             (message "get issue note error %s" data)))))

(defun carlos/gitlab-fetch-assigen-feature-single-project (project-id cur_page_index history-lists callback)
  ;; (interactive "")
  (setq carlos/gitlab-fetch-assigen-feature-single-project-cur-page-index cur_page_index)
  (setq carlos/gitlab-fetch-assigen-feature-single-project-project-id project-id)
  (setq carlos/gitlab-fetch-assigen-feature-single-project-history-lists history-lists)
  (setq carlos/gitlab-fetch-assigen-feature-single-project-callback callback)
  (request
   (concat carlos/gitlab-default-gitlaburl "/projects/" project-id "/issues")
   :type "GET"
   :parser 'json-read
   :data (list (cons "page" cur_page_index) (cons "per_page"  carlog/gitlab-query-qa-bug-issue-pagesize) (cons "state" "opened") (cons "scope" "assigned-to-me"))
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "fetch data length is%s detect value :%s" (length data) (< (length data) carlog/gitlab-query-qa-bug-issue-pagesize ))
               (if (< (length data) carlog/gitlab-query-qa-bug-issue-pagesize )
                   (progn
                     (message "fetch data end")
                     (let ((total-history (vconcat carlos/gitlab-fetch-assigen-feature-single-project-history-lists data)))

                       (funcall carlos/gitlab-fetch-assigen-feature-single-project-callback total-history)
                       )
                     )
                 (progn
                   (message "try to fetch data recur")
                   (carlos/gitlab-fetch-assigen-feature-single-project carlos/gitlab-fetch-assigen-feature-single-project-project-id
                                                                       (+ 1 carlos/gitlab-fetch-assigen-feature-single-project-cur-page-index)
                                                                       (vconcat carlos/gitlab-fetch-assigen-feature-single-project-history-lists data)
                                                                       carlos/gitlab-fetch-assigen-feature-single-project-callback
                                                                       )
                   )
                 )
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "fetch data error:%s" data)
             ))))

(defun carlos/gitlab-fetch-assigne-feature (project-lists history callback)
  ;; (interactive )
  (message "call carlos/gitlab-fetch-assigne-feature")
  (setq gitlab-project-id (cdr (cdr (car project-lists))))
  (setq carlos/gitlab-fetch-assinge-feature-lists (cdr project-lists))
  (setq carlos/gitlab-fetch-assigne-feature-callback callback)
  (setq carlos/gitlab-fetch-assigne-feature-history history)
  (carlos/gitlab-fetch-assigen-feature-single-project gitlab-project-id 0 history (lambda (history) (interactive "")

                                                                                    (message "dump history length is:%s detect left projects is:%s" (length history) (length carlos/gitlab-fetch-assinge-feature-lists))
                                                                                    (if (< 0 (length carlos/gitlab-fetch-assinge-feature-lists))
                                                                                        (progn
                                                                                          (message "recur call")
                                                                                          (carlos/gitlab-fetch-assigne-feature carlos/gitlab-fetch-assinge-feature-lists history carlos/gitlab-fetch-assigen-feature-single-project-callback)
                                                                                          )
                                                                                      (progn

                                                                                        (mapcar '(lambda (data) (interactive "")
                                                                                                   (message "debug data is:%s" (assoc 'web_url data))
                                                                                                   (browse-url (cdr (assoc 'web_url data)))
                                                                                                   )
                                                                                                history
                                                                                                )
                                                                                        )))))

(defun carlos/gitlab-recursion-check-issue-states (issue-list callback)
  "docstring"
  (let ((src-item (car issue-list)))
    (let ((src-info (nth 0 src-item))
          (mention-body (nth 1 src-item))
          (carlos-mention-issue-count (nth 2 src-item))
          (left-issue-list (cdr issue-list))
          )

      (carlos/get-issue-state src-info mention-body
                              (lambda (state src-info)
                                (if (equal state "closed")
                                    (progn
                                      (if (< 0 (length left-issue-list))
                                          (carlos/gitlab-recursion-check-issue-states left-issue-list callback)
                                        (progn
                                          (carlos/gitlab-labled-comment-handled-QA-issue state src-info callback)
                                          )))
                                  (progn
                                    (message "issue in mention-body :%s state is:%s" mention-body state)
                                    ;; (funcall callback )
                                    (funcall callback (list "1" "2"))
                                    )))))))

;; (defun carlos/gitlab-project-product-feature-list ()
;;   (interactive)
;;   (carlos/gitlab-fetch-assigne-feature gitlab-feature-project-list (vector) (lambda () (interactive "") )))

(defun carlos/gitlab-bot-auto-labeled-fixed-issue (project-id issue-iid src-info callback)
  ;; (message "bot auto labeled fix issue debug prj:%s issue:%s" project-id issue-iid)
  (carlos/gitlab-get-issue-notes-all project-id issue-iid 1 90 (vector) src-info
                                     (lambda (result src-info) (interactive "")

                                       (let ((founded nil)
                                             (carlos-mention-issue-count 0 )
                                             (closed-count 0))
                                         ;; (message "result length is:%s" (length result))
                                         (setq mentioned-issue (delq nil
                                                                     (mapcar
                                                                      (lambda (note) (interactive "")
                                                                        (if (and (string-match "mentioned in issue.*" (cdr (assoc 'body note)))
                                                                                 (string-match "carlos" (cdr (assoc 'username  (cdr (assoc 'author note)))))
                                                                                 )
                                                                            (progn
                                                                              (let ((msg-body (split-string (nth 1 (split-string (cdr (assoc 'body note)) "mentioned in issue ")) "#") )
                                                                                    (mention-body (cdr (assoc 'body note)))
                                                                                    )

                                                                                (let ((issue-parsed (split-string (nth 1 (split-string mention-body "mentioned in issue ")) "#")))

                                                                                  (let ((project-path (nth 0 issue-parsed))
                                                                                        (issue-id (nth 1 issue-parsed)))
                                                                                    (if (< 0 (length project-path))
                                                                                        (progn
                                                                                          ;; (message "found project path issue-passed:%s" issue-parsed)
                                                                                          ;; (setq founded t)
                                                                                          (setq carlos-mention-issue-count (+ 1 carlos-mention-issue-count))
                                                                                          (list src-info mention-body carlos-mention-issue-count)
                                                                                          )
                                                                                      (progn
                                                                                        ;; (message "not found project-path issue-parsed:%s" issue-parsed)
                                                                                        nil ))))))
                                                                          (progn
                                                                            nil
                                                                            )))
                                                                      result)))
                                         (message "Get mentioned comment length is:%s" (length mentioned-issue) )
                                         (carlos/gitlab-recursion-check-issue-states mentioned-issue callback)
                                         ;; (funcall callback (list "1" "2"))
                                         ))))

(defun carlos/gitlab-labled-comment-handled-QA-issue (state src-info callback)

  (let ((issue-iid (int-to-string (cdr (assoc 'iid src-info))))
        (project-id (int-to-string (cdr (assoc 'project_id src-info))))
        (web-url (cdr (assoc 'web_url src-info)))
        (issue-labels (cdr (assoc 'labels src-info)))
        (author (cdr  (assoc 'username  (cdr (assoc 'author src-info)))))
        )
    (if (equal author "ghost")
        (setq author (carlos/util-random-choose-time (list "luoyuening" "yinjing")) ))
    (let ((new-labels (format "%s,softdev-fixed" (s-join "," issue-labels)))
          )

      (carlos/gitlab-issue-add-note project-id issue-iid (format "\/assign @%s \n\n 请确认是否正确修复 @%s \n\n\n* 如果修复请直接关闭 \n* 如果未修复请 @ 我，并描述问题同时写上测试的版本号" author author) new-labels
                                    (lambda (new-labels project-id issue-iid) (interactive "")

                                      (if (not (equal new-labels nil))
                                          (progn
                                            (carlos/gitlab-issue-add-label project-id issue-iid new-labels callback)

                                            ;; (funcall callback (list "1" "2"))
                                            )
                                        (progn

                                          ;; (carlos/gitlab-issue-add-label project-id issue-iid new-labels callback)
                                          (funcall callback (list "1" "2"))
                                          )))))))

(defun carlos/get-issue-state (src-info issue-path-id  callback)
  (let ((issue-parsed (split-string (nth 1 (split-string issue-path-id "mentioned in issue ")) "#")))
    ;; (message "debug get issue state issue-parsed is:%s" issue-parsed)
    (let ((project-path (nth 0 issue-parsed))
          (issue-id (nth 1 issue-parsed)))
      (request
       (concat carlos/gitlab-default-gitlaburl "/projects/" (url-hexify-string project-path) "/issues/" issue-id)
       :type "GET"
       :data (list (cons "page" 1) (cons "per_page" 90) )
       :parser 'json-read
       :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
       :success
       (cl-function
        (lambda (&key data &allow-other-keys)

          (funcall callback (cdr (assoc 'state data)) src-info)
          ))
       :error
       (cl-function
        (lambda (&key data &allow-other-keys)

          (funcall callback nil src-info)))))))

(defun carlos/gitlab-issue-add-note  (project-id issue-iid note-body new-labels callback)
  ;; (message "debug carlos gitlab issue add note")
  ;; (message "Try to add note:%s url is:%s" note-body (concat carlos/gitlab-default-gitlaburl "/projects/" project-id "/issues/" issue-iid "/notes" ))
  (request
   (concat carlos/gitlab-default-gitlaburl "/projects/" project-id "/issues/" issue-iid "/notes" )
   :type "POST"
   :data (list (cons "body" note-body) )
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success
   (cl-function
    (lambda (&key data &allow-other-keys)
      (message "Noted Issue id:%s project_id:%s Body:%s" issue-iid project-id note-body)

      (funcall callback new-labels project-id issue-iid)
      ))
   :error
   (cl-function
    (lambda (&key data &allow-other-keys)
      (message "carlos/gitlab-issue-add-note error is:%s" data)
      (funcall callback nil nil nil)
      ))))

(defun carlos/gitlab-issue-add-label (project-id issue-iid new-labels callback)
  ;; (setq new-labels (format "%s,softdev-in-process" (s-join "," issue-labels)))
  (message "try to label issue")
  (request
   (concat carlos/gitlab-default-gitlaburl (format  "/projects/%s/issues/%s" project-id issue-iid))
   :type "PUT"
   :parser 'json-read
   :data (list (cons "labels" new-labels))
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "try labeled issue success")
               (funcall callback (list "1" "2"))

               ;; (funcall callback)
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "try labeled issue error is:%s" data)
             (funcall callback (list "1" "2"))
             ;; (funcall callback)
             ))))

(defun carlos/gitlab-convert-issue-result-source-format (issue-gitlab)
  "docstring"
  (let ((i issue-gitlab))
    (let ((converted-item ""))
      (cl-loop for j across (cdr (assq 'labels i))
               do (progn
                    (setq converted-item (concat (decode-coding-string j 'utf-8-emacs) " " converted-item))
                    (setq carlos/gitlab-del-issue-worker-label (decode-coding-string j 'utf-8-emacs))
                    ))
      (setq converted-item (concat converted-item (replace-regexp-in-string "\n" "" (concat (cdr (assq 'state i))  " " (decode-coding-string (cdr (assq 'title i)) 'utf-8-emacs)))))
      (setq search-source
            (append search-source
                    (list (cons converted-item (list  (cdr (assq 'web_url i)) (cdr (assq 'state i)) (cdr (assq 'id i)) (cdr (assq 'iid i)) (cdr (assq 'project_id i)) converted-item carlos/gitlab-del-issue-worker-label) )))))))

(defun carlos/gitlab-update-query-issue-cache (carlos/gitlab-projectid  new-issue-data)
  "docstring"
  (let ((new-source-item (carlos/gitlab-convert-issue-result-source-format new-issue-data))
        (old-source-list (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues)))
    (if (boundp 'carlos/gitlab-query-last-issues)
        (setcdr (assoc carlos/gitlab-projectid carlos/gitlab-query-last-issues) (append old-source-list new-source-item)))))

(defun carlos/gitlab-filter-label-opened-over-due-issues (data)
  "docstring"
  (let ((worker-lists (carlos/gitlab-convert-worker-list carlos/gitlab-work-labels)))
    (delq nil (mapcar
               (lambda (value)

                 (let ((match-length (length  (mapcar
                                               (lambda (label)
                                                 (let ((label-cur (decode-coding-string label 'utf-8-emacs)))

                                                   (setq carlos/debug-contain-list worker-lists)
                                                   (setq carlos/debug-contain-item label-cur)

                                                   (if (-contains? worker-lists label-cur)
                                                       value
                                                     nil)
                                                   ))
                                               (cdr (assoc 'labels value))))))
                   ;; (let ((due_date  (cdr (assoc 'due_date value))))
                   ;;   (messag "debug due_date is:%s" due_date)
                   ;;   )
                   (message "due_date is:%s" (cdr (assoc 'due_date value)))
                   (if (and (< 0 match-length) (not (equal nil (cdr (assoc 'due_date value)))))
                       (progn
                         (let ((due_date (carlos/parse-time (cdr (assoc 'due_date value))))
                               (today (carlos/parse-time (format-time-string "%Y-%m-%d"))))
                           (message "due date is:%s" due_date)
                           (if (time-less-p due_date today)
                               value
                             nil)
                           )
                         )
                     nil)))
               data ))))

(defun carlos/gitlab-query-opened-issue-with-labels (page-size page-offset label history callback)
  "docstring"
  (message "Querying page:%s page-size:%s" page-offset page-size)
  (request
   (url-encode-url (concat carlos/gitlab-default-gitlaburl (format "/issues?state=opened&labels=%s&per_page=%d&page=%d" label page-size page-offset)))
   :type "GET"
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success
   (cl-function
    (lambda (&key data &allow-other-keys)
      (if (<  (length data) page-size)
          (funcall callback (vconcat history data))
        (progn
          (carlos/gitlab-query-opened-issue-with-labels page-size (+ 1 page-offset) label (vconcat history data) callback)))))
   :error
   (cl-function
    (lambda (&key data &allow-other-keys)
      (message "Query issue with label:%s error:%s" label data)
      (funcall callback nil nil nil)))))

(defun carlos/gitlab-convert-worker-list (worker-list)
  "docstring"
  (mapcar (lambda (data) (interactive "")
            (message "data is:%s" (nth 1 data))
            (nth 1 data))

          worker-list))

(setq carlos/gitlab-handle-overdue-issues-lists nil)

(defun carlos/gitlab-handle-overdue-issues ()
  "docstring"
  (interactive "")
  (if (equal nil carlos/gitlab-handle-overdue-issues-lists)
      (progn
        (if (equal t (y-or-n-p "Start Fetch over due issues?"))
            (carlos/gitlab-query-opened-issue-with-labels 90 1 "" (list)
                                                          (lambda (data) (interactive "")
                                                            (message "history data length is:%s" (length data))
                                                            (setq carlos/debug-cache-query-opened-not-labeld-issue data)
                                                            (let ((parsed-data (carlos/gitlab-filter-label-opened-over-due-issues data)))

                                                              (if (>= 0 (length parsed-data))
                                                                  (progn
                                                                    (terminal-notifier-notify "carlos gitlab" "winner winner chicken dinner ovuer due issues" "com.carlos.gitlaboverdue")
                                                                    (message "winner winner chicken dinner" ))
                                                                (progn
                                                                  (message "start hanle over due issues")
                                                                  (setq carlos/gitlab-handle-overdue-issues-lists parsed-data)
                                                                  (carlos/gitlab-handle-overdue-issues)
                                                                  ))
                                                              )
                                                            ))))
    (progn
      (message "dump over due issue lists is:%s" (length carlos/gitlab-handle-overdue-issues-lists))
      (let ((result carlos/gitlab-handle-overdue-issues-lists))
        (let ((browse-range 10))
          (if (<= (length result) browse-range)
              (progn
                (mapcar (lambda (data)
                          (message "dump url is:%s" (assoc 'web_url data))
                          (browse-url (cdr (assoc 'web_url data)))
                          )
                        result)
                (message "browse web url end")
                (terminal-notifier-notify "carlos gitlab " "winner winner chicken dinner ovuer due issues" "com.carlos.gitlaboverdue")
                (setq carlos/gitlab-handle-overdue-issues-lists nil)
                )
            (progn
              (let ((handled-urls (subseq result 0 10))
                    (left-urls (subseq result 10 (length result))))
                (setq carlos/gitlab-handle-overdue-issues-lists left-urls)
                (terminal-notifier-notify (format  "剩余待处理 overdue issue:%s" (length carlos/gitlab-handle-overdue-issues-lists) )  "overdue issue 提醒" "org")
                (mapcar (lambda (data)
                          (browse-url (cdr (assoc 'web_url data)))

                          )
                        handled-urls)))))))))

(defun carlos/gitlab-browser-feature-request ()
  "查询 assign 给我的 issue 主要是除了 QA 之外的问题"
  (interactive "")
  (if (equal carlos/gitlab-get-assign-without-qa-cached nil)
      (progn
        (if (equal t (y-or-n-p "Start Fetch assign issue not qa?"))
            (carlos/gitlab-query-api "/issues?scope=assigned-to-me&state=opened&" 1 90 (vector)
                                     (lambda (data)
                                       (let ((filted-issues (delq nil (mapcar (lambda (issue)
                                                                                (let ((web_url (cdr (assoc 'web_url issue))))

                                                                                  (if (or (string-match ".*[qQ][aA].*" web_url)
                                                                                          (string-match ".*softdev-department-stuff.*" web_url))
                                                                                      (progn
                                                                                        nil
                                                                                        )
                                                                                    (progn
                                                                                      issue
                                                                                      ))))
                                                                              data))))
                                         (setq carlos/gitlab-get-assign-without-qa-cached
                                               (carlos/gitlab-sort-issues-by-planed-created-label (carlos/gitlab-sort-issues-by-priority-lables filted-issues))
                                               )
                                         (setq carlos/debug-gitlab-get-assign-without-qa-filted-issues filted-issues)
                                         (carlos/gitlab-browser-feature-request)
                                         )))))
    (progn
      (let ((result carlos/gitlab-get-assign-without-qa-cached))
        (let ((browse-range 10))
          (if (<= (length result) browse-range)
              (progn
                (terminal-notifier-notify "carlos gitlab " "最后一页需要处理的需求评审" "com.carlos.gitlabassignotqa")
                (mapcar (lambda (data)
                          ;; (message "dump result labels is:%s" (assoc 'labels data))
                          (browse-url (cdr (assoc 'web_url data)))
                          (sleep-for 0.1)
                          )
                        (reverse result))
                (terminal-notifier-notify "carlos gitlab " "winner winner chicken dinner ovuer due issues" "com.carlos.gitlaboverdue")
                (setq carlos/gitlab-get-assign-without-qa-cached nil)
                )
            (progn
              (let ((handled-urls (subseq result 0 10))
                    (left-urls (subseq result 10 (length result))))
                (terminal-notifier-notify "carlos gitlab " (format "剩余需要处理的需求评审:%s" (length left-urls) ) "com.carlos.gitlabassignotqa")
                (setq carlos/gitlab-get-assign-without-qa-cached left-urls)
                (mapcar (lambda (data)
                          (browse-url (cdr (assoc 'web_url data)))
                          (sleep-for 0.1)
                          )
                        (reverse handled-urls))))))))))

(defun carlos/gitlab-parse-milestone-include-issue (milestone-str)
  "docstring"
  (delq nil (mapcar (lambda (data)
                      ;; (message "dump parse milestone include issue:%s" data)
                      ;; (if (and (not (string-match ".*[qQ][aA].*" data)) (string-match "http://.*" data))
                      (if (and t (string-match "http://.*" data))
                          data
                        nil
                        ))
                    (split-string milestone-str ))))

(defun carlos/gitlab-parse-add-milestone-web-url (data)
  "docstring"
  (let ((match-proejct-url (delq nil (mapcar (lambda (value)

                                               (if (equal (int-to-string (cdr (assoc 'project_id data))) (cdr (cdr value)))
                                                   (car (cdr value))
                                                 nil)
                                               )
                                             gitlab-project-sources
                                             ))))

    (if (< 0 (length match-proejct-url))
        (progn
          (format "http://112.74.81.51:10088/%s/milestones/%d" (nth 0 match-proejct-url) (cdr (assoc 'iid data)))
          )
      nil)))

(defun carlos/gitlab-add-notify-to-milestone-include-issue (argv data)
  "docstring"
  (message "Try to parse valid-url argv:%s" argv)
  (let ((found-valid-url nil))
    (unwind-protect
        (progn
          (setq carlos/debug-gitlab-add-notify-to-milestone-include-issue-data data)
          (setq carlos/debug-gitlab-add-notify-to-milestone-include-issue-argv argv)
          (let ((valid-url (carlos/gitlab-parse-milestone-include-issue argv))
                (milestone-url (carlos/gitlab-parse-add-milestone-web-url data))
                (project-id (nth 0 (carlos/gitlab-parse-project-id-issue-iid-from-web-url argv)))
                (issue-iid (nth 1 (carlos/gitlab-parse-project-id-issue-iid-from-web-url argv)))
                )
            (setq found-valid-url t)
            (if (or (<= 0 (length  project-id)) (<= 0 (length issue-iid)))
                (setq found-valid-url nil)
                )
            (message "Found valid-url is:%s" (equal valid-url t))
            (if (and valid-url (< 0 (length valid-url)))
                (progn
                  (message "开始自动通知产品部" )
                  (mapcar (lambda (url)
                            (let ((found-project-id  (delq nil (mapcar (lambda (value)

                                                                         (if (equal project-id (car (cdr value)))
                                                                             (cdr (cdr value))
                                                                           nil)
                                                                         )
                                                                       gitlab-project-sources
                                                                       ))))
                              ;; (message "dump found project id is:%s" found-project-id)
                              (if (< 0 (length found-project-id))
                                  (progn
                                    ;; (message "found project id is:%s  issue iid is:%s" found-project-id (trim-string issue-iid))
                                    (carlos/gitlab-issue-add-note (nth 0 found-project-id) (trim-string issue-iid) (format "@kzd @qjx @hanjinbo @lgd 这是安排的开发里程碑 %s " milestone-url) nil
                                                                  (lambda (new-labels project-id issue-iid)
                                                                    (interactive "")
                                                                    ))
                                    )
                                (progn
                                  (terminal-notifier-notify "carlos gitlab " "没有找到匹配的项目 id" "com.carlos.gitlabautocommentproductissue")
                                  )
                                )))
                          valid-url)
                  )
              (message "里程碑没有包含引用的需求文档链接" )))
      ;; (progn
          )
      ;;   (if (equal t found-valid-url)
      ;;       (progn
      ;;         (message "Wait for auto comment result" ))
      ;;     (progn
      ;;       (message "The milestone is not include issue url" )
      ;;       )))
      )))

(defun carlos/gitlab-parse-project-id-issue-iid-from-web-url (web-url)
  "docstring"
  (let ((issue-split (split-string web-url "/issues/")))
    (let ((project-split (split-string (or (nth 0 issue-split) "") ":10088/")))
      (let ((real-issue (split-string (or (nth 1 issue-split) ""))))
        (let ((issue-remove-note (split-string (or (nth 0 real-issue) "") "#note")))
          (message "remove note is:%s" issue-remove-note)
          (message "project split is:%s issue-splite %s" (nth 1 project-split) (nth 0 real-issue))
          (list (nth 1 project-split) (nth 0 issue-remove-note))
          )
        )
      )))

(defun carlos/gitlab-query-milestone-for-add-issue (argv history index heading callback)
  "docstring"
  ;; (interactive "")
  (message "Fetch milesetone Index:%s history len:%s" index (length history))
  (request
   (format (concat carlos/gitlab-default-gitlaburl "/projects/%s/milestones?") argv)
   :type "GET"
   :data (list (cons "page" index) (cons "per_page" "90")  (cons "order_by" "updated_at") (cons "sort" "desc"))
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "get queried milestones length:%s" (length data))
               (if (< (length data) 90)
                   (progn
                     (setq data (vconcat data history))
                     (setq data (sort data (lambda (data1 data2)
                                             (time-less-p  (carlos/parse-time (cdr (assoc 'due_date data2))) (carlos/parse-time (cdr (assoc 'due_date data1))))
                                             )))
                     (let ((sort_milestones '())
                           (select-project-id argv))
                       (cl-loop for i across data
                                do(progn
                                    (setq sort_milestones (append sort_milestones (list (cons (concat (decode-coding-string (cdr (assoc 'title i)) 'utf-8-emacs) " " (cdr (assoc 'due_date i))) (cons  (cdr (assoc 'id i)) (cdr (assoc 'due_date i)))))))
                                    ))

                       (message "debug argv is:%s" argv)
                       (message "after parsed milestones choose project id is:%s" select-project-id)
                       (if (not (equal nil (assoc select-project-id carlos/gitlab-query-last-milestone)))
                           (setcdr (assoc select-project-id carlos/gitlab-query-last-milestone) sort_milestones)
                         (setq carlos/gitlab-query-last-milestone (append  carlos/gitlab-query-last-milestone (list (cons select-project-id sort_milestones))))
                         )

                       ;; (if (not (equal nil (assoc select-project-id carlos/gitlab-query-last-milestone-timestamp)))
                       ;;     (setcdr (assoc select-project-id carlos/gitlab-query-last-milestone-timestamp) (time-to-seconds))
                       ;;   (setq carlos/gitlab-query-last-milestone-timestamp (append carlos/gitlab-query-last-milestone-timestamp (list (cons select-project-id (time-to-seconds)))))
                       ;;   )
                       (message "Debug helm choose gitlab milestone")
                       (setq choose-milestone (helm :sources (helm-build-sync-source "helm-gitlab-milestone"
                                                               :candidates  sort_milestones
                                                               :fuzzy-match t
                                                               :action (helm-make-actions "Close Selected Issue"   (lambda (candidate)
                                                                                                                     (message "carlos/gitlab-query-milestone-for-add-issue choose milesetone is:%s" candidate)
                                                                                                                     candidate
                                                                                                                     )
                                                                                          "Open Select Issue"  (lambda (candidate)

                                                                                                                 candidate
                                                                                                                 )
                                                                                          ))
                                                    :buffer "*helm gitlab projects*"
                                                    :input heading
                                                    ))
                       (message "try to call callback select project id:%s choose-milestone is:%s callback is:%s" select-project-id choose-milestone callback)
                       (funcall callback choose-milestone select-project-id)))
                 (progn
                   (setq history (vconcat data history))
                   (carlos/gitlab-query-milestone-for-add-issue argv history (+ 1 index) heading callback)
                   ))
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "查询里程碑失败:%s" data)
             (funcall callback nil nil)
             ))))

(defun carlos/gitlab-Query-projects-list-issue (project-list cur_page_index page_size history callback)
  "docstring"
  (interactive "P")
  (let ((project-id (cdr  (cdr (car project-list))))
        (left-project-lists project-list)
        )
    (request
     (concat carlos/gitlab-default-gitlaburl "/projects/" project-id "/issues")
     :type "GET"
     :parser 'json-read
     :data (list (cons "page" cur_page_index) (cons "per_page"  page_size) (cons "state" "opened"))
     :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (message "Get Project Issuses project id:%s get data length:%s Cur page:%s" project-id (length data) cur_page_index)
                 (setq history (vconcat history data))
                 ;; (message "after concat history is:%s" (length history))
                 (cond ((> page_size (length data))
                        (progn
                          ;; (message "debug code before left project list length is:%s type is:%s" (length project-list) (type-of project-list))
                          (setq left-project-lists (cdr project-list))
                          ;; (message "left project lists length is:%s" (length left-project-lists))
                          (if (< 0 (length left-project-lists))
                              (progn
                                (carlos/gitlab-Query-projects-list-issue left-project-lists 1 page_size history callback)
                                )
                            (progn
                              (funcall callback history))
                            )
                          )
                        ;; (carlos/gitlab-queried-project-issues data carlos/gitlab-qa-bug-project-list plan-or-not)
                        )
                       (t
                        (progn
                          (carlos/gitlab-Query-projects-list-issue left-project-lists (+ 1 cur_page_index) page_size history callback))))))
     :error (cl-function
             (lambda (&key data &allow-other-kesy)
               (message "query issue error %s" data))))))

(defun carlos/gitlab-filter-not-assign-issue (issues-list)
  "docstring"
  (delq nil (mapcar (lambda (issue)
                      (let (
                            (assigne (cdr  (assoc 'username  (cdr  (assoc 'assignee issue)))))
                            (labels-list (append (cdr (assoc 'labels issue)) nil))
                            )
                        (if (and (equal nil assigne ) (equal (-contains-p labels-list "not-softdev") nil))
                            (progn
                              (message "debug labels list is:%s assigne is:%s" labels-list assigne)
                              issue
                              )
                          (progn
                            nil
                            ;; (if (and (equal nil assigne) (equal (-contains-p labels-list "not-softdev") nil))
                            ;;     issue
                            ;;   nil)
                            ))))
                    issues-list)))

(defun carlos/gitlab-convert-issues-list-to-buffer (issues)
  "docstring"
  (let ((buffer-name "* QA not assign"))
    (get-buffer-create (format "%s" buffer-name))
    (kill-buffer (format "%s" buffer-name))
    (switch-to-buffer (format "%s" buffer-name))
    (org-mode)
    (insert "* IssueList [0/0]")
    (mapc (lambda (data)
            (setq show-labels "Labels: ")
            (mapcar '(lambda (label)
                       (setq show-labels (concat show-labels  (decode-coding-string label 'utf-8-emacs) ", " ))
                       )
                    (cdr (assoc 'labels data)))
            (setq carlos/gitlab-qa-bug-print-issue (format "\n** TODO 未 assign: %s %s\n  %s \n author:%s \n assigne:%s  \n\"%s\""
                                                           (and (cdr (assoc 'web_url data)) (decode-coding-string (cdr (assoc 'web_url data)) 'utf-8-emacs))
                                                           (and (cdr (assoc 'title data)) (decode-coding-string (cdr (assoc 'title data)) 'utf-8-emacs))
                                                           show-labels
                                                           (cdr  (assoc 'name  (cdr  (assoc 'author data))))
                                                           (cdr  (assoc 'username  (cdr  (assoc 'assignee data))))
                                                           (and (cdr (assoc 'description data)) (replace-regexp-in-string "[*\"]" ""  (decode-coding-string (cdr (assoc 'description data)) 'utf-8-emacs)))
                                                           ))
            (insert carlos/gitlab-qa-bug-print-issue)
            )
          issues)
    (org-update-parent-todo-statistics)
    )
  )

(setq carlos/gitlab-not-assign-qa-issue-list nil)

(defun carlos/gitlab-map-browser-weburl (src-list)
  "docstring"
  (message "call map browser weburl src list length is:%s" (length src-list))
  (mapcar (lambda (issue)
            (message "debug broser web_url:%s" (cdr (assoc 'web_url issue)))
            (browse-url (cdr (assoc 'web_url issue)))
            (sleep-for 0.1)
            )
          src-list)
  )

(defun carlos/gitlab-open-list-by-step (src-list)
  "docstring"
  (if (<= (length src-list) 6)
      (progn
        (carlos/gitlab-map-browser-weburl src-list)
        (setq carlos/gitlab-not-assign-qa-issue-list nil)
        (carlos/gitlab-QA-inbox)
        )
    (progn
      (setq web-list (subseq src-list 0 10))
      (carlos/gitlab-map-browser-weburl web-list)
      (setq carlos/gitlab-not-assign-qa-issue-list (subseq src-list 10 (length src-list)))
      (terminal-notifier-notify "carlos gitlab " (format "剩余需要处理的not assign QA:%s" (length (subseq src-list 10 (length src-list))) ) "com.carlos.gitlabassignotqa")
      )
    )
  )

(defun carlos/gitlab-leju-QA ()
  "docstring"
  (interactive "")
  (if (not (equal nil carlos/gitlab-not-assign-qa-issue-list))
      (carlos/gitlab-open-list-by-step carlos/gitlab-not-assign-qa-issue-list)
    (carlos/gitlab-Query-projects-list-issue gitlab-project-QA-BUG  1 90 (vconcat) (lambda (history) (interactive "")
                                                                                     ;; (message "history length is:%s" (length history))
                                                                                     (let (
                                                                                           (not-assign-list (carlos/gitlab-filter-not-assign-issue history)))
                                                                                       (carlos/gitlab-open-list-by-step not-assign-list))))))

(defun carlos/gitlab-check-link-planed-status (plan-files-lists history search-item)
  "docstring"
  ;; (interactive )
  (let ((plan-file (cdr (car plan-files-lists)))
        (left-plan-files (cdr plan-files-lists)))
    ;; (message "left-plan-files length is:%s plan-file is:%s" (length left-plan-files) plan-file)
    (if (<= (length plan-files-lists) 0)
        (progn
          ;; (message "Debug history is:%s" history)
          history
         )
      (progn
        (setq history (append history (delq 2 (org-map-entries
                                                 (lambda ()
                                                   (let ((outline-path (org-get-outline-path))
                                                         (found 2)
                                                         (heading (org-get-heading nil nil))
                                                         (todo-state (org-get-todo-state))
                                                         )
                                                     (mapc (lambda (item)
                                                             (if (string-match-p (format ".*%s.*" search-item ) item)
                                                                 (progn
                                                                   ;; (message "debug heading length:%s data:%s" (length outline-path) outline-path)
                                                                   ;; (message "item is:%s" item)
                                                                   (if (and
                                                                        (string-match-p (format ".*%s.*" search-item ) item)
                                                                        (not (string-match-p "测试 .*" heading))
                                                                        )
                                                                       (progn
                                                                         ;; (message "found match item :%s item search-item:%s" (org-get-heading nil nil) search-item)
                                                                         (if (equal "TODO" todo-state)
                                                                             (setq found 0)
                                                                           (setq found 1))
                                                                         )
                                                                     (progn
                                                                       (setq found 2)
                                                                       )
                                                                     )
                                                                   )
                                                                 (setq found 2))
                                                             )
                                                           outline-path)
                                                     found
                                                     ))
                                                 t (list plan-file)))))
        (carlos/gitlab-check-link-planed-status left-plan-files history search-item)
        ))
    ))
(defun carlos/gitlab-query-feature-planed ()
  "docstring"
  (interactive)
  (let ((search-item (read-string "Link:")))
    (carlos/gitlab-feature-query-url search-item)
    )
  )

(defun carlos/gitlab-feature-query-url (url &optional callback)
  "docstring"
  (let ((check-list (carlos/gitlab-check-link-planed-status gitlab-week-bot-list (list ) url)))
    ;; (message "carlos/gitlab-query-feature-planed checklist is:%s " check-list)
    (let (
          (check-list-length (length check-list))
          (remove-unplan (length (delq 0 check-list)))
          )
      ;; (message "remove-unplan is:%s plan is:%s" remove-unplan check-list-length)
      (if (or (not (eq remove-unplan check-list-length))
              (= 0 check-list-length))
          (progn
            (if callback
                (funcall callback "planing" url))
            (kill-new " ")
            (message "Plan is not all planed"))
        (progn
          (if callback
              (funcall callback "planed" url))
          (kill-new "@qjx 开发工单已经安排完毕\n\n\/assign @qjx ")
          (message "All planed"))))))

(defun carlos/gitlab-issue-assign-to-me ()
  "docstring"
  (interactive)
  (if carlos/gitlab-issues-assign-to-me-cached
      (progn
        (let ((browse-issues (subseq carlos/gitlab-issues-assign-to-me-cached 0 (min 10 (length carlos/gitlab-issues-assign-to-me-cached)))))
          (mapcar (lambda (item) (interactive)
                    (browse-url (cdr (assoc 'web_url item)))
                    (sleep-for 0.168)
                    )
                  (reverse browse-issues)
                  )
          (if (<= (length carlos/gitlab-issues-assign-to-me-cached) 10)
              (progn
                (setq carlos/gitlab-issues-assign-to-me-cached nil)
                (terminal-notifier-notify "读取 assign 给我的工单" "工单处理完毕"  "com.carlos.gitlab")
                )
            (progn
              (terminal-notifier-notify "读取 assign 给我的工单" (format "剩余待处理工单:%s" (- (length carlos/gitlab-issues-assign-to-me-cached) (length (subseq carlos/gitlab-issues-assign-to-me-cached 0 10))))   "com.carlos.gitlab")
              (setq carlos/gitlab-issues-assign-to-me-cached (subseq carlos/gitlab-issues-assign-to-me-cached 10 (length carlos/gitlab-issues-assign-to-me-cached)))
              ))))
    (progn
      (if (yes-or-no-p "上一轮数据已处理完毕是否启动一轮新的读取？")
          (carlos/gitlab-query-api "/issues?scope=assigned-to-me&state=opened&" 1 90 (vector)
                                   (lambda (data)
                                     (message "Fetch issue assigned to me length is:%s" (length data))
                                     (let ((sorted-issues (carlos/gitlab-sort-issues-by-priority-lables  (carlos/gitlab-sort-issues-by-planed-created-label
                                                                                                          (carlos/gitlab-sort-issues-by-created-timestamp  data)))))
                                       (setq carlos/gitlab-issues-assign-to-me-cached sorted-issues)
                                       (carlos/gitlab-issue-assign-to-me)
                                       )))))))

(setq carlos/gitlab-issues-assign-to-me-cached nil)

(defun carlos/gitlab-get-user-and-plan-time-from-param (args)
  "docstring"
  (setq carlos/gitlab-debug-gitlab-get-user-and-plan-time-from-param-args args)
  (list (cdr (assoc "labels" args))
        (cdr (assoc "due_date" args))))

(defun carlos/gitlab-get-QA-Bug-link (src-str)
  (interactive )
  (let ((splited (split-string src-str " ")))
    ;; (message "get first item of splited is:%s" (car splited))
    (if (or (string-equal (car splited) "处理") (string-equal (car splited) "修复"))
        (progn
          ;; (message "found fix string")
          (setq links (list (nth 0 (cdr splited))))
          ;; (mapcar (lambda (data)
          ;;           ;; (message "mapcar data is:%s" data)
          ;;           (setq links (append links (list  data)))
          ;;           )
          ;;         (cdr splited))
          (message "parsed links is:%s" links)
          (carlos/gitlab-query-qa-bug-update-links links)
          ;; (carlos/gitlab-query-qa-bug-update-links (list "http://112.74.81.51:10088/QA/QA/issues/202" "http://112.74.81.51:10088/QA/QA/issues/203"))
          )
      (progn
        (if carlos/gitlab-use-org-to-auto-plan
            (progn
              (switch-to-buffer "*Org Agenda*")
              ;; (outline-next-heading)
              (evil-next-visual-line )
              ))
        ))))

(defun carlos/gitlab-get-link-info (link callback)
  ;; (interactive)
  (setq carlos/gitlab-get-link-info-callback callback)
  ;; (message "carlos start get link info")
  (let ((splited (split-string link "/")))
    (let ((group (nth 3 splited))
          (project-name (nth 4 splited))
          (issue-iid (nth 6 splited))
          )
      (setq carlos/gitlab-get-link-info-issue-iid issue-iid)
      (mapcar (lambda (data)
                ;; (message "map data of project sources is:%s project-name is:%s" data (format "%s/%s" group project-name))
                (if (string-equal (nth 0 data) (format "%s/%s" group project-name))
                    (progn
                      (message "Query:%s" (format (concat carlos/gitlab-default-gitlaburl "/projects/%s/issues?iids=%s") (cdr (cdr data)) issue-iid))
                      ;; (message "get project data is:%s " (cdr (cdr data)))
                      ;; (message "try to get issue is:%s" (format (concat carlos/gitlab-default-gitlaburl "/projects/%s/issues?iids[]=%s") (cdr (cdr data)) issue-iid ))
                      (request
                       (format (concat carlos/gitlab-default-gitlaburl "/projects/%s/issues?iids=%s") (cdr (cdr data)) issue-iid)
                       :type "GET"
                       :parser 'json-read
                       :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
                       :success (cl-function
                                 (lambda (&key data &allow-other-keys)
                                   (setq issue-id (cdr (assoc 'id (elt data 0))))
                                   (setq issue-labels (cdr (assoc 'labels (elt data 0))))
                                   (setq issue-iid (cdr (assoc 'iid (elt data 0))))
                                   (setq issue-project-id (cdr (assoc 'project_id (elt data 0))))
                                   (message "debug contains labels:%s labels is:%s typeof labels is:%s" (seq-contains issue-labels "softdev-in-process") issue-labels (type-of issue-labels))
                                   (if (not (seq-contains issue-labels "softdev-in-process"))
                                       (setq new-labels (format "%s,softdev-in-process" (s-join "," issue-labels)))
                                     (setq new-labels (format "%s" (s-join "," issue-labels)))
                                     )
                                   (message "PUTING:%s" (concat carlos/gitlab-default-gitlaburl (format  "/projects/%s/issues/%s" issue-project-id issue-iid)))
                                   ;; (message "dump new labels is:%s" new-labels)
                                   ;; (message "update issues labels is:%s" (concat carlos/gitlab-default-gitlaburl (format  "/projects/%s/issues/%s" issue-project-id issue-iid)))
                                   (request
                                    (concat carlos/gitlab-default-gitlaburl (format  "/projects/%s/issues/%s" issue-project-id issue-iid))
                                    :type "PUT"
                                    :parser 'json-read
                                    :data (list (cons "labels" new-labels))
                                    :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
                                    :success (cl-function
                                              (lambda (&key data &allow-other-keys)
                                                (message "Update Fix Issue label Success Try to Note source QA issue")
                                                ;; (message "try to call add note" )
                                                ;; (message "dump issue labels is:%s" new-labels)
                                                ;; (message "project-id is:%s issue-iid:%s" issue-project-id issue-iid)
                                                (carlos/gitlab-issue-add-note (format "%d" issue-project-id)  (format "%d" issue-iid)  "\/assign @Softdev-QA-bot \n carlos assign to plan bot \n"  nil
                                                                              (lambda (new-labels project-id issue-iid) (interactive "")
                                                                                (if carlos/gitlab-use-org-to-auto-plan
                                                                                    (progn
                                                                                      (switch-to-buffer "*Org Agenda*")
                                                                                      ;; (outline-next-heading)
                                                                                      (evil-next-visual-line )
                                                                                      ))
                                                                                ))
                                                ;; (message "call back is:%s" carlos/gitlab-get-link-info-callback)
                                                (funcall carlos/gitlab-get-link-info-callback)
                                                ))
                                    :error (cl-function
                                            (lambda (&key data &allow-other-keys)
                                              (message "Update issue error:%s" data)
                                              (if carlos/gitlab-use-org-to-auto-plan
                                                  (progn
                                                    (switch-to-buffer "*Org Agenda*")
                                                    ;; (outline-next-heading)
                                                    (evil-next-visual-line )
                                                    )
                                                  )
                                              (funcall carlos/gitlab-get-link-info-callback)
                                              ))
                                    )
                                   )
                                 )
                       :error (cl-function
                               (lambda (&key data &allow-other-keys)
                                 (message "query issue error %s" data)
                                 ))
                       )
                      )
                  ))
              gitlab-project-sources
              )
      )))

(defun carlos/gitlab-query-qa-bug-update-links (links)
  (interactive)
  (setq carlos/gitlab-query-qa-bug-update-links-links links)
  (message "Handling qa bug links is:%s" links)
  (carlos/gitlab-get-link-info (car links) (lambda ()
                                             (if (< 0 (length (cdr carlos/gitlab-query-qa-bug-update-links-links)))
                                                 (carlos/gitlab-query-qa-bug-update-links (cdr carlos/gitlab-query-qa-bug-update-links-links))
                                               (progn

                                                 )
                                               ))))

(defun carlos/gitlab-query-project-opened-issues (page-index page-size project-id history callback)
  (interactive)
  (message "Get project opened issue index:%s size:%s history length:%s project id is is:%s" page-index page-size (length history) project-id)
  (setq carlos/gitlab-query-project-opened-issue-page-index page-index)
  (setq carlos/gitlab-query-project-opened-issue-page-size page-size)
  (setq carlos/gitlab-query-project-opened-issue-project-id project-id)
  (setq carlos/gitlab-query-project-opened-issue-history history)
  (setq carlos/gitlab-query-project-opened-issue-callback callback)
  ;; (message "before start query %s" (format (concat carlos/gitlab-default-gitlaburl  "/projects/%s/issues?state=opened") carlos/gitlab-query-project-opened-issue-project-id))
  (request
   (format (concat carlos/gitlab-default-gitlaburl  "/projects/%s/issues?state=opened") carlos/gitlab-query-project-opened-issue-project-id)
   :type "GET"
   :parser 'json-read
   :data (list (cons "page" carlos/gitlab-query-project-opened-issue-page-index) (cons "per_page"  carlos/gitlab-query-project-opened-issue-page-size))
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               ;; (message "Get Data")
               ;; (message "get project issue is:%s pagesize is:%s" (length data) carlos/gitlab-query-project-opened-issue-page-size)
               (cond ((> carlos/gitlab-query-project-opened-issue-page-size (length data))
                      (progn
                        (let ((history (vconcat data carlos/gitlab-query-project-opened-issue-history)))
                          ;; (message "issue end call callback callback is:%s" carlos/gitlab-query-opened-issue-finish-callback)
                          (funcall carlos/gitlab-query-project-opened-issue-callback history)
                          )
                        )
                      )
                     (t
                      (progn
                        (carlos/gitlab-query-project-opened-issues (+ 1carlos/gitlab-query-project-opened-issue-page-index )
                                                                   carlos/gitlab-query-project-opened-issue-page-size
                                                                   vconcat carlos/gitlab-query-project-opened-issue-history data
                                                                   carlos/gitlab-query-opened-issue-finish-callback)
                        )))
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "query project issues fail error:%s" data)
             ))))

(defun carlos/gitlab-query-api (api page-index page-size history callback &optional token)
  (interactive)
  (message "Try Get project opened issue index:%s size:%s history length:%s" page-index page-size (length history))
  (message "Url is:%s" (concat carlos/gitlab-default-gitlaburl  api (format "page=%d&per_page=%d" page-index page-size)))
  (request
   (concat carlos/gitlab-default-gitlaburl  api (format "page=%d&per_page=%d" page-index page-size))
   :type "GET"
   :parser 'json-read
   ;; :data (list (cons "page" page-index) (cons "per_page"  page-size) (cons "order_by"  "created_at") (cons "sort"  "desc"))
   :headers (list (cons "PRIVATE-TOKEN"  (or token carlos/gitlab-default-gitlaburl-token )))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               ;; (setq calros/debug/gitlab/gitlab-query-api/data data)
               ;; (setq data calros/debug/gitlab/gitlab-query-api/data)
               (message "carlos/gitlab-query-api data length:%s page size is:%s" (length data) page-size)
               (cond ((> page-size (length data))
                      (progn
                        (let ((history (vconcat data history)))
                          (funcall callback history))))
                     (t
                      (progn
                        (carlos/gitlab-query-api
                         api
                         (+ 1 page-index)
                         page-size
                         (vconcat history data)
                         callback))))))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "query project issues fail error:%s" data)))))

(defun carlos/gitlab-query-api-with-token (api token page-index page-size history callback)
  (interactive)
  (message "Try Get project opened issue index:%s size:%s history length:%s" page-index page-size (length history))
  (message "Url is:%s" (concat carlos/gitlab-default-gitlaburl  api (format "page=%d&per_page=%d" page-index page-size)))
  (request
   (concat carlos/gitlab-default-gitlaburl  api (format "page=%d&per_page=%d" page-index page-size))
   :type "GET"
   :parser 'json-read
   ;; :data (list (cons "page" page-index) (cons "per_page"  page-size) (cons "order_by"  "created_at") (cons "sort"  "desc"))
   :headers (list (cons "PRIVATE-TOKEN"  (or token carlos/gitlab-default-gitlaburl-token)))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (cond ((> page-size (length data))
                      (progn
                        (let ((history (vconcat data history)))
                          (funcall callback history))))
                     (t
                      (progn
                        (carlos/gitlab-query-api
                         api
                         (+ 1 page-index)
                         page-size
                         (vconcat history data)
                         callback))))))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "carlos/gitlab-query-api-with-token fail error:%s" data)))))

(defun carlos/gitlab-browse-mergerequest ()
  "处理 Merge request, 其中过滤掉了已安排的工单"
  (interactive)
  (if (not  (equal nil carlos/gitlab-browse-todos-history))
      (progn
        (setq carlos/gitlab-browse-todos-result carlos/gitlab-browse-todos-history)
        (if (<= (length carlos/gitlab-browse-todos-result) carlos/gitlab-browse-todos-offset)
            (progn
              (setq carlos/gitlab-browse-todos-offset 0)
              (setq carlos/gitlab-browse-todos-history nil)
              (terminal-notifier-notify "浏览todo提醒" "已浏览完毕，重头开始" "org.hi")
              )
          (progn
            (message "offset it less than todo result length:%s" (length carlos/gitlab-browse-todos-result))
            (let ((start carlos/gitlab-browse-todos-offset)
                  (end (+ (+ 0 carlos/gitlab-browse-todos-pagesize) carlos/gitlab-browse-todos-offset))
                  )
              (if (<= (length carlos/gitlab-browse-todos-result) end)
                  (progn
                    (let ((end (length carlos/gitlab-browse-todos-result)))
                      (progn
                        (mapcar (lambda (item)
                                  ;; (message "debug item is:%s" item)
                                  (if (not (equal nil item))
                                      ;; (browse-url item)
                                      (progn
                                        (browse-url-default-macosx-browser item)
                                        (sleep-for carlos/browser-url-sleep)
                                        )
                                    ))
                                (reverse (seq-subseq carlos/gitlab-browse-todos-result start end))))))
                (progn
                  (mapcar (lambda (item)
                            ;; (message "carlos/gitlab-browse-mergerequest item is:%s checked is:%s" item (not (equal nil item)))
                            (if (not (equal nil item))
                                (progn
                                  (browse-url-default-macosx-browser item)
                                  (sleep-for carlos/browser-url-sleep)
                                  )
                              ))
                          (reverse (seq-subseq carlos/gitlab-browse-todos-result start end))))))
            (setq carlos/gitlab-browse-todos-offset (+ carlos/gitlab-browse-todos-pagesize carlos/gitlab-browse-todos-offset))
            ;; (message "next gitlab browse todos offset is:%s/%d" carlos/gitlab-browse-todos-offset  (length carlos/gitlab-browse-todos-result))
            (terminal-notifier-notify "查看 TODO 进度 " (format "next gitlab browse todos offset is:%s/%d" carlos/gitlab-browse-todos-offset  (length carlos/gitlab-browse-todos-result))  "com.carlos.gitlab")
            )))
    (progn
      (carlos/gitlab-query-api "/merge_requests?scope=assigned-to-me&order_by=created_at&state=opened&sort=asc&" 1 90 (vector)
                               (lambda (data)
                                 (setq carlos/gitlab-browse-todos-result
                                       (mapcar (lambda (item)
                                                 ;; (message "item is:%s" item)
                                                 (cdr  (assoc 'web_url item)))
                                               data))
                                 (setq carlos/gitlab-browse-todos-history carlos/gitlab-browse-todos-result)
                                 (carlos/gitlab-browse-mergerequest)
                                 )))))

(defun carlos/gitlab-browse-todos ()
  "docstring"
  (interactive "")
  (if (not (equal nil carlos-cached-todo-history))
      (progn
        (carlos/gitlab-browse-cached-todos)
        )
    (progn
      (if (yes-or-no-p "是否启动查询:")
          (progn
            (carlos/gitlab-query-api "/todos?order_by=updated_at&state=opened&sort=asc&" 1 90 (vector)
                                     (lambda (data)
                                       (message "Get issue todo data length:%s" (length data))
                                       (let ((result nil))
                                         (setq result
                                               (delq nil (mapcar (lambda (item)
                                                                   (cdr  (assoc 'target_url item)))
                                                                 data)))
                                         (setq carlos-cached-todo-history result)
                                         (carlos/gitlab-browse-cached-todos)
                                         ))))))))

(defun carlos/gitlab-browse-cached-todos ()
  "docstring"
  (interactive "")
  (message "debug browse cached todos is:%s" (length carlos-cached-todo-history))
  (if (> 10 (length carlos-cached-todo-history))
      (progn
        (mapcar (lambda (item)
                  ;; (browse-url item)
                  (browse-url-default-macosx-browser item)
                  (sleep-for carlos/browser-url-sleep)
                  )
                (reverse carlos-cached-todo-history))
        (setq carlos-cached-todo-history nil)
        (terminal-notifier-notify "查看 TODO 进度 " "处理完毕"  "com.carlos.gitlab")
        )
    (progn
      (let ((url-list (seq-subseq carlos-cached-todo-history 0 10)))
        (mapcar (lambda (item)
                  ;; (browse-url item)
                  (browse-url-default-macosx-browser item)
                  (sleep-for carlos/browser-url-sleep)
                  )
                (reverse url-list)))
      (setq carlos-cached-todo-history (seq-subseq carlos-cached-todo-history 10 (length carlos-cached-todo-history)))
      (terminal-notifier-notify "查看 TODO 进度 " (format "剩余待处理 todo:%s" (length carlos-cached-todo-history))  "com.carlos.gitlab"))))

(defun carlos/gitlab-handle-all-fetch-todos (data)
  "docstring"
  (interactive "")
  (setq carlos/gitlab-browse-todos-result
        (delq nil (mapcar (lambda (item)
                            ;; (message "item is:%s" item)
                            (let ((labels (cdr (assoc 'labels item)))
                                  (web_url (cdr (assoc 'web_url item))))
                              (setq carlos/debug-filter-issue-labels labels)
                              (if (or (-contains? (append labels nil) "softdev-in-process") (string-match ".*[qQ][aA].*" web_url))
                                  (progn
                                    ;; (message "debug filter softdev in process %s" labels)
                                    nil
                                    )
                                (cdr (assoc 'web_url item))))
                            ;; (cdr  (assoc 'web_url item))
                            )
                          data)))

  (setq carlos/gitlab-browse-todos-history (append carlos/gitlab-browse-todos-history carlos/gitlab-browse-todos-result))

  (if (<= (length carlos/gitlab-browse-todos-history) carlos/gitlab-browse-todos-offset)
      (progn
        (setq carlos/gitlab-browse-todos-offset 0)
        (setq carlos/gitlab-browse-todos-history nil)
        (terminal-notifier-notify "浏览提醒" "已浏览完毕，重头开始" "org.hi"))
    (progn
      (let ((start carlos/gitlab-browse-todos-offset)
            (end (+ (+ 0 carlos/gitlab-browse-todos-pagesize) carlos/gitlab-browse-todos-offset))
            )
        (if (<= (length carlos/gitlab-browse-todos-history) end)
            (progn
              (let ((end (length carlos/gitlab-browse-todos-history)))
                (progn
                  (mapcar (lambda (item)
                            ;; (browse-url item)
                            (browse-url-default-macosx-browser item)
                            (sleep-for carlos/browser-url-sleep)
                            )
                          (reverse (seq-subseq carlos/gitlab-browse-todos-history start end))))))
          (progn
            (mapcar (lambda (item)
                      ;; (browse-url item)
                      (browse-url-default-macosx-browser item)
                      (sleep-for carlos/browser-url-sleep)
                      )
                    (reverse (seq-subseq carlos/gitlab-browse-todos-history start end)))))
        (terminal-notifier-notify "查看进度 " (format "next gitlab browse todos offset is:%s/%d" end  (length carlos/gitlab-browse-todos-history))  "com.carlos.gitlab")
        )
      (setq carlos/gitlab-browse-todos-offset (+ (+ 0 carlos/gitlab-browse-todos-pagesize) carlos/gitlab-browse-todos-offset)))))

(defun carlos/gitlab-query-todos (page-index page-size history callback)
  (interactive)
  (message "Get project opened issue index:%s size:%s history length:%s" page-index page-size (length history))
  (setq carlos/gitlab-query-todos--page-index page-index)
  (setq carlos/gitlab-query-todos--page-size page-size)
  (setq carlos/gitlab-query-todos--history history)
  (setq carlos/gitlab-query-todos--callback callback)
  (request
   (concat carlos/gitlab-default-gitlaburl  "/todos")
   :type "GET"
   :parser 'json-read
   :data (list (cons "page" carlos/gitlab-query-todos--page-index) (cons "per_page"  carlos/gitlab-query-todos--page-size))
   :headers (list (cons "PRIVATE-TOKEN"  carlos/gitlab-default-gitlaburl-token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "get data length:%s page size is:%s" (length data) carlos/gitlab-query-todos--page-size)
               (cond ((> carlos/gitlab-query-todos--page-size (length data))
                      (progn
                        (let ((history (vconcat data carlos/gitlab-query-todos--history)))
                          ;; (message "issue end call callback callback is:%s" carlos/gitlab-query-opened-issue-finish-callback)
                          (funcall carlos/gitlab-query-todos--callback history)
                          )
                        )
                      )
                     (t
                      (progn
                        (message "try recur call")
                        (carlos/gitlab-query-todos (+ 1 carlos/gitlab-query-todos--page-index )
                                                   carlos/gitlab-query-todos--page-size
                                                   (vconcat carlos/gitlab-query-todos--history data)
                                                   carlos/gitlab-query-todos--callback)
                        )))
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "query project issues fail error:%s" data)
             ))))

(defun carlos/gitlab-clean-merged-branchs (token project-id &optional callback)
  "docstring"
  (interactive)
  (request
   (concat carlos/gitlab-default-gitlaburl  "/projects/" project-id "/repository/merged_branches")
   :type "DELETE"
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "Error: carlos/gitlab-clean-merged-branchs:%s" data)
             ))))

(defun carlos/gitlab-accept-merge-reqeust (token project-id target-id)
  "docstring"
  (interactive)
  ;; PUT http://112.74.81.51:10088/api/v4/projects/211/merge_requests/11/merge?should_remove_source_branch=t&merge_commit_message=debug123
  (request
   (concat carlos/gitlab-default-gitlaburl  "/projects/" project-id "/merge_requests/" target-id "/merge")
   :type "PUT"
   :parser 'json-read
   :headers (list (cons "PRIVATE-TOKEN"  token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "carlos/gitlab-clean-merged-branchs:%s" data)
             ))))

(defun carlos/gitlab-week-bot-map (projects)
  (setq project (car projects))
  (if (not (file-exists-p (cdr project)))
      (progn
        (if (yes-or-no-p (format "文件:%s 不存在，是否继续处理:?[y/n]" (cdr project)))
            (progn
              (setq carlos/gitlab-week-bot-map-left-project (cdr projects))
              (carlos/gitlab-week-bot-map carlos/gitlab-week-bot-map-left-project)
              )))
    (progn
      (switch-to-buffer (find-file (cdr project)))
      (outline-show-all)
      (setq carlos/gitlab-projectid (cdr  (car project)))
      (setq carlos/gitlab-week-bot-map-left-project (cdr projects))
      (message "Querying Data From Internet...")
      (carlos/create-gitlab-project-issue-list 0 (vector) (lambda (keep-going) (interactive "")
                                                            (message "Keep-going is:%s" keep-going)
                                                            (if (equal nil keep-going)
                                                                (progn
                                                                  )
                                                              (progn
                                                                ;; (message "left project length is:%s" (length carlos/gitlab-week-bot-map-left-project))
                                                                (if (< 0 (length carlos/gitlab-week-bot-map-left-project))
                                                                    (progn
                                                                      (carlos/gitlab-week-bot-map carlos/gitlab-week-bot-map-left-project)
                                                                      (message "Auto Next Project"))
                                                                  (progn
                                                                    (message "Parsed End try to call carlos/gitlab-project-report-bot" )
                                                                    (carlos/gitlab-project-report-bot)
                                                                    )))))))))

(defun carlos/gitlab-project-report (projects)
  (if (gnus-buffer-exists-p "* Project Report *")
      (kill-buffer "* Project Report *")
      )

  (switch-to-buffer "* Project Report *")
  (goto-char (point-max))
  (let (value)
    (dolist (project projects value)
      ;; (message "elt is:%s" project)
      (insert (format "项目: %s\n" (car  (car project))))
      (save-excursion
        (switch-to-buffer (find-file (cdr project)))
        (setq carlos/gitlab-projectid (cdr  (car project)))
        (goto-char 0)
        (setq carlos-done-count 0)
        (setq carlos-todo-count 0)
        (setq carlos-working-count 0)
        (setq carlos-todo-test 0)
        (setq dev-todo-count 0)
        (setq dev-doing-count 0)
        (setq test-todo-count 0)
        (setq test-doing-count 0)
        (progn
          (org-map-entries
           (lambda ()
             ;; (message "entries is:%s" (org-get-heading))
             (let ((todo-state (org-get-todo-state))
                   (heading (org-get-heading t t))
                   )
               ;; (message "TODO state is:%s if done is:%s " todo-state (string-equal "DONE" todo-state))
               (cond ((string-equal "DONE" todo-state)
                      (setq carlos-done-count (+ carlos-done-count 1))
                      )
                     ((string-equal "LABELED" todo-state)
                      (progn
                        (setq carlos-working-count (+ carlos-working-count 1))
                        (if (carlos-gitlab-dev-process/check-dev-issue heading)
                            (setq dev-doing-count (+ 1 dev-doing-count)))
                        (if (carlos-gitlab-dev-process/check-test-issue heading)
                            (setq test-doing-count (+ 1 test-doing-count)))
                        )
                      )
                     ((string-equal "TODO" todo-state)
                      (progn
                        (if (carlos-gitlab-dev-process/check-dev-issue heading)
                            (setq dev-todo-count (+ 1 dev-todo-count)))
                        (if (carlos-gitlab-dev-process/check-test-issue heading)
                            (setq test-todo-count (+ 1 test-todo-count)))
                        ;; (message "heading is:%s" heading)
                        (if (string-match "测试 " heading)
                            (progn
                              ;; (message "heading is test")
                              (setq carlos-todo-test (+ carlos-todo-test 1))
                              )
                          (progn
                            ;; (message "heading is not test")
                            (setq carlos-todo-count (+ carlos-todo-count 1))
                            )))))))
           t 'file)
          ))
      (insert (format  "已完成工单:%s 正在进行的开发工单:%s 未安排的开发工单:%s 正在进行的测试工单:%s 未安排的测试工单:%s\n\n" carlos-done-count dev-doing-count dev-todo-count test-doing-count test-todo-count))
      (insert "\n\n")))
  (carlos/gitlab-query-assigned-issues-by-group (list "3" "19") (list))
  (switch-to-buffer "* Project Report *")
  ;; (split-window-right)a
  
  ;; (message "Sync leju project status done auto update gitlab wiki")
  )

(defun carlos/gitlab-project-report-bot ()
  (interactive)
  (carlos/gitlab-project-report gitlab-week-bot-list))

(defun carlos/gitlab-week-bot-auto-sync ()
  "docstring"
  (interactive )
  (let ((worktree-git-status (carlos-git-worktree-status "/Users/carlos/Documents/leju/leju_prj/project-plan-platform")))
    (if (equal 1 worktree-git-status)
        (progn
          (if (equal 0 (shell-command "cd /Users/carlos/Documents/leju/leju_prj/project-plan-platform && git pull origin master"))
              (carlos/gitlab-week-bot-map gitlab-week-bot-list)
            (terminal-notifier-notify "同步项目" "同步项目失败"  "com.carlos.gitlab")
              )
          )
      (progn
        (terminal-notifier-notify "同步项目" "项目文件未提交"  "com.carlos.gitlab")
        ))))

(defun carlos/org-get-project-name-local-value ()
  "docstring"
  (interactive "")
  ;; (message "value is:%s" (and (boundp 'carlos/plan-org-project-name) carlos/plan-org-project-name))
  (and (boundp 'carlos/plan-org-project-name) carlos/plan-org-project-name)
  )

(defun carlos/parse-heading-is-fix-qa (headingstr)
  "docstring"
  (let ((splited (split-string headingstr " ")))
    ;; (message "get first item of splited is:%s" (car splited))
    (if (or (string-equal (car splited) "处理") (string-equal (car splited) "修复"))
        t
      (progn
        nil
        ))))

(defun carlos/list-labeled-entries ()
  (interactive)
  (delq nil (org-map-entries
             (lambda ()
               (message "current entry is:%s" (org-get-heading))
               )
             "+TODO=\"LABELED\"" (list (buffer-file-name (current-buffer))))))

;;; -*- lexical-binding: t -*-

(setq carlos/recently-clock-list nil)

(defun carlos/debug-thing-at-point-line (args)
  "docstring"
  (interactive "P")
  (message "debug carlos thing at point line :%s" (replace-regexp-in-string "*" ""
                                                                            (replace-regexp-in-string "LABELED" "" (thing-at-point 'line)))))

(defun write-string-to-file (string file)
  (interactive)
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min)
                    (point-max)
                    file))))

(defun carlos/query-issue-str ()
  (setq query-str (carlos/get-mark-term))
  (if (or (eq query-str nil) (string= query-str ""))
      (progn
        (setq query-str (replace-regexp-in-string "TODO" ""
                                                  (replace-regexp-in-string "ADD" ""
                                                                            (replace-regexp-in-string "DONE" ""
                                                                                                      (replace-regexp-in-string "*" ""
                                                                                                                                (replace-regexp-in-string "LABELED" "" (thing-at-point 'line))))))))))

(defun carlos/get-mark-term ()
  (interactive)
  (if mark-active
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
            (message "")
          (message selection))
        )
    )
  )

(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "”" " " (replace-regexp-in-string "“" " "  (replace-regexp-in-string "\"" " "(replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))))
  )

;; (message "debug trim string:%s" (trim-string " 测试 android aelos 版本 1.3.2-8-gb1024fd 英文模式: 1.预置动作“JINGGLEBELLS”\"The ugly ducking\"\"the Emperors\"s new clothes\"在动作商城是未下载状态，如果出现该情况为有问题 2.卸载应用，手机系统切换到西班牙文，看预置的动作名称是否为英文动作名"))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; (message "carlos debug vector to list %s"
;;          (carlos/debug-vector-to-list (read
;;                                        (get-string-from-file  "/Users/carlos/Downloads/emacslist-debugvector.txt"))))

(defun carlos/get-date-from-calendar ()
  (interactive)
  (save-excursion
    (message "%s" (org-read-date))
    ))

;; (message "debug concat list %s" (append  (list (cons "1"  2) (cons "2"  3)) (list (cons "3"  4) (cons "4" 5))))

(defun carlos/get-mark-str (beg end)
  "message region or \"empty string\" if none highlighted"
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (message "%s" (if (and beg end)
                    (buffer-substring-no-properties beg end)
                  "empty string")))

(defun carlos/org_get_full_outling_heading ()
  (message "%s" (concat (mapconcat 'identity (org-get-outline-path) "❂")  "➤" (org-get-heading t t) )))

(defun carlos/org_get_full_outling_without_heading ()
  (message "%s" (concat (mapconcat 'identity (org-get-outline-path) "❂")  "➤" )))

(defun carlos/org-get-project-name-local-value ()
  "docstring"
  (interactive "")
  ;; (message "value is:%s" (and (boundp 'carlos/plan-org-project-name) carlos/plan-org-project-name))
  (and (boundp 'carlos/plan-org-project-name) carlos/plan-org-project-name)
  )

(defun carlos/funs-convert-string-to-year-week (src)
  "docstring"
  (let ((parsed-time (carlos/parse-time src)))
    (let ((format-week-year (format-time-string "%Y.%W" parsed-time)))
      ;; (message "format-week-year is:%s" format-week-year)
      format-week-year
      )))

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace repeated blank lines to just 1.
Only space and tab is considered whitespace here.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+\n" nil "move")
            (replace-match "\n")))
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))
        (progn
          (goto-char (point-max))
          (while (equal (char-before) 32) ; char 32 is space
            (delete-char -1))))
      (message "white space cleaned"))))

(defun xah-fix-datetime-stamp (@input-string &optional @begin-end)
  "Change timestamp under cursor into a yyyy-mm-dd format.
If there's a text selection, use that as input, else use current line.

Any “day of week”, or “time” info, or any other parts of the string, are discarded.
For example:
 「TUESDAY, FEB 15, 2011 05:16 ET」 ⇒ 「2011-02-15」
 「November 28, 1994」              ⇒ 「1994-11-28」
 「Nov. 28, 1994」                  ⇒ 「1994-11-28」
 「11/28/1994」                     ⇒ 「1994-11-28」
 「1994/11/28」                     ⇒ 「1994-11-28」

When called in lisp program, the optional second argument “*begin-end” is a vector of region boundary. (it can also be a list)
If “*begin-end” is non-nil, the region is taken as input (and “*input-string” is ignored).

URL `http://ergoemacs.org/emacs/elisp_parse_time.html'
Version 2015-04-14"

  (interactive
   (list nil (vector (line-beginning-position) (line-end-position))))

  (let (
        ($str (if @begin-end (buffer-substring-no-properties (elt @begin-end 0) (elt @begin-end 1)) @input-string))
        ($work-on-region-p (if @begin-end t nil)))
    (require 'parse-time)

    (setq $str (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" $str)) ; remove white spaces

    (setq $str
          (cond
           ;; USA convention of mm/dd/yyyy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" $str)
            (concat (match-string 3 $str) "-" (match-string 1 $str) "-" (match-string 2 $str)))
           ;; USA convention of m/dd/yyyy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" $str)
            (concat (match-string 3 $str) "-0" (match-string 1 $str) "-" (match-string 2 $str)))

           ;; USA convention of mm/dd/yy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $str)
            (concat (format-time-string "%C") (match-string 3 $str) "-" (match-string 1 $str) "-" (match-string 2 $str)))
           ;; USA convention of m/dd/yy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $str)
            (concat (format-time-string "%C") (match-string 3 $str) "-0" (match-string 1 $str) "-" (match-string 2 $str)))

           ;; yyyy/mm/dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" $str)
            (concat (match-string 1 $str) "-" (match-string 2 $str) "-" (match-string 3 $str)))

           ;; some ISO 8601. yyyy-mm-ddThh:mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T[0-9][0-9]:[0-9][0-9]" $str)
            (concat (match-string 1 $str) "-" (match-string 2 $str) "-" (match-string 3 $str)))
           ;; some ISO 8601. yyyy-mm-dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)" $str)
            (concat (match-string 1 $str) "-" (match-string 2 $str) "-" (match-string 3 $str)))
           ;; some ISO 8601. yyyy-mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)" $str)
            (concat (match-string 1 $str) "-" (match-string 2 $str)))

           ;; else
           (t
            (progn
              (setq $str (replace-regexp-in-string "January " "Jan. " $str))
              (setq $str (replace-regexp-in-string "February " "Feb. " $str))
              (setq $str (replace-regexp-in-string "March " "Mar. " $str))
              (setq $str (replace-regexp-in-string "April " "Apr. " $str))
              (setq $str (replace-regexp-in-string "May " "May. " $str))
              (setq $str (replace-regexp-in-string "June " "Jun. " $str))
              (setq $str (replace-regexp-in-string "July " "Jul. " $str))
              (setq $str (replace-regexp-in-string "August " "Aug. " $str))
              (setq $str (replace-regexp-in-string "September " "Sep. " $str))
              (setq $str (replace-regexp-in-string "October " "Oct. " $str))
              (setq $str (replace-regexp-in-string "November " "Nov. " $str))
              (setq $str (replace-regexp-in-string "December " "Dec. " $str))

              (setq $str (replace-regexp-in-string "\\([0-9]+\\)st" "\\1" $str))
              (setq $str (replace-regexp-in-string "\\([0-9]+\\)nd" "\\1" $str))
              (setq $str (replace-regexp-in-string "\\([0-9]+\\)rd" "\\1" $str))
              (setq $str (replace-regexp-in-string "\\([0-9]\\)th" "\\1" $str))

              (let (dateList $year $month $date $yyyy $mm $dd )
                (setq dateList (parse-time-string $str))
                (setq $year (nth 5 dateList))
                (setq $month (nth 4 dateList))
                (setq $date (nth 3 dateList))

                (setq $yyyy (number-to-string $year))
                (setq $mm (if $month (format "%02d" $month) "" ))
                (setq $dd (if $date (format "%02d" $date) "" ))
                (concat $yyyy "-" $mm "-" $dd))))))

    (if $work-on-region-p
        (progn (delete-region  (elt @begin-end 0) (elt @begin-end 1))
               (insert $str))
      $str )))

(defun carlos/parse-heading-is-fix-qa (headingstr)
  "docstring"
  (let ((splited (split-string headingstr " ")))
    ;; (message "get first item of splited is:%s" (car splited))
    (if (or (string-equal (car splited) "处理") (string-equal (car splited) "修复"))
        t
      (progn
        nil
        ))))

(defun carlos/gitlab-issues-merged-request (token project-id target-id note &optional callback)
  "docstring"
  (interactive)
  (request
   (url-encode-url (concat carlos/gitlab-default-gitlaburl  "/projects/" project-id "/issues/" target-id "/notes"))
   :type "POST"
   :parser 'json-read
   :data (list (cons "body" note))
   :headers (list (cons "PRIVATE-TOKEN"  token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (if callback
                   (funcall callback data))))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "Error: carlos/gitlab-issues-merged-request :%s" data)))))

(defun carlos/gitlab-comment-merged-request (token project-id target-id note &optional callback)
  "docstring"
  (interactive)
  (request
   (concat carlos/gitlab-default-gitlaburl  "/projects/" project-id "/merge_requests/" target-id "/notes")
   :type "POST"
   :parser 'json-read
   :data (list (cons "body" note) )
   :headers (list (cons "PRIVATE-TOKEN"  token))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (if callback
                   (funcall callback data))
               ))
   :error (cl-function
           (lambda (&key data &allow-other-keys)
             (message "Error: carlos/gitlab-comment-merged-request:%s" data)
             ))))

(defun carlos/gitlab-is-include-label (labels-list item)
  "docstring"
  ;; (interactive)
  (seq-contains labels-list item (lambda (item1 item2)
                                   (equal item1 (decode-coding-string item2 'utf-8-emacs)))))

(defun carlos/util-random-choose-time (src-list)
  "docstring"
  (nth (random (length src-list)) src-list)
  ;; (interactive)
  )

(defun carlos-git-worktree-status (worktree)
  "1 is clean, 0 is change not staged for commit"
  (interactive)
  (shell-command (format "cd %s && git status  | grep \"Changes not staged for commit\"" worktree))
  )

(defun carlos/gitlab-issues-assign-to (user-id callback)
  "docstring"
  (interactive "")
  (carlos/gitlab-query-api (format "/issues?assignee_id=%s&state=opened&scope=all&" user-id)  1 90 (vector)
                           (lambda (data)
                             (funcall callback data)
                             )))

(defun carlos/gitlab-query-assigned-issues-by-group (group history)
  "docstring"
  (interactive)
  (message "group is:%s" group)
  (let ((user_id (car group)))
    (message "user id is:%s" user_id)
    (if (not (equal nil user_id))
        (carlos/gitlab-issues-assign-to user_id (lambda (issues)
                                                  (let ((labels (append issues nil) ))
                                                    (carlos/gitlab-query-assigned-issues-by-group (cdr group) (append labels history))
                                                    )
                                                  ))
      (progn
        (let (
              (count-values (list (cons (list "P1") 0 )
                                  (cons (list  "P1" "softdev-accept") 0 )
                                  (cons (list "P1" "softdev-plan-created") 0)
                                  (cons (list "P2") 0)
                                  (cons (list  "P2" "softdev-accept") 0 )
                                  (cons (list "P2" "softdev-plan-created") 0)
                                  ))
              (count-values-title (list (cons (list "P1") "P1 需求")
                                        (cons (list "P1" "softdev-accept") "P1 已确认需求")
                                        (cons (list "P1" "softdev-plan-created") "P1 已建立开发工单需求")
                                        (cons (list "P2") "P2 需求")
                                        (cons (list "P2" "softdev-accept") "P2 已确认需求")
                                        (cons (list "P2" "softdev-plan-created") "P2 已建立开发工单需求")
                                        ))
              )
          (let ((issues-labels (mapcar (lambda (issue)
                                         (let ((issue-labels (append (cdr (assoc 'labels issue)) nil)))
                                           (mapc (lambda (count-item)
                                                   (if (carlos/list-contain-list issue-labels (car count-item))
                                                       (progn
                                                         (setcdr (assoc (car count-item) count-values) (+ 1 (cdr count-item))))))
                                                 count-values)))
                                       history)))
            (message "dump count values is:%s" count-values)
            (let ((output (mapcar (lambda (value)
                                    (format "%s: %s" (cdr (assoc (car value) count-values-title)) (cdr value))
                                    )
                                  count-values)))
              (insert (s-join "\n\n" output))
              (switch-to-buffer "* Project Report *")
              (write-file "/tmp/leju_project_report.txt")
              (if carlos/gitlab-mail-project-update-ctrl
                  (shell-command "cd /Users/carlos/Documents/system-config/tools/export_leju_project && node index.js &" "*Export Leju Prj*")))))))))

;; (carlos/gitlab-query-assigned-issues-by-group (list "3" "19") (list))

(defun carlos/list-contain-list (src-list sub-list)
  "docstring"
  (let ((result (delq nil(mapcar (lambda (item)
                                   (if (-contains-p src-list item)
                                       nil
                                     item)
                                   )
                                 sub-list))))
    (= (length result) 0)
    ))

(provide 'carlos-gitlab)

